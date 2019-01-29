% Filename: PullGPX_CreateFeatures.m
%
% Author: Sam Maticka
%
% Description: this script reads in a gpx file and creates variables
clear;clc;close all;

% Specs for the GPX files
Athletes     = {'Adam','Sam'}; % Athletes to pull activities from
ActivityType = {'swim','ride','run','walk'}; % Activity types to pull
FolderBase   = ['/Users/smaticka/Box Sync/stanford/Classes',...
    '/MS&E 226 small data/mini project/I_Data_Files/']; % Location of athlete folders

% Thresholds for removing data points
BinPoints   = 90;  % points in each time-avg bin. 90 is 9-10 minutes ish.
SwimThresh  = 1.9; % m/s, Michael Phelps swam for 200 meters
RunThresh   = 11;  % m/s, 11 m/s = 24.5mph. Bolt ran 28mph for 100m.. downhill, hmm
DispThresh  = 100;

% loop through each athlete and pull GPX data for each activity
for ii = 1:length(Athletes)
    % Athlete to pull data from
    Athlete = Athletes{ii}; 
    
    % loop through each activity type and pull the GPX data
    for jj = [1,4]%:length(ActivityType)
        % Activity type to pull data for
        Activity = ActivityType{jj};

        cd([FolderBase, Athlete, '_activities/',ActivityType{jj}])

        clear activities
                
        % get list of files in the directory to upload data from
        allFiles  = dir('*.gpx');
        FileNames = {allFiles.name};
        
        % loop through each file and convert GPX to MATLAB variables
        for kk = 1:length(FileNames)
            
            FileName = FileNames{kk};
            FileID   = FileName(1:15);
            
            % Read in GPS data
            data = gpxread(FileName, 'FeatureType', 'track');
            
            % pull variables from map data structure
            Lat       = data.Latitude;
            Lon       = data.Longitude;
            Elevation = data.Elevation; % meters, elevation
            
            % make into column vectors
            Lat       = Lat(:);
            Lon       = Lon(:);
            Elevation = Elevation(:);
            
            % Get Start Time of Activity
            Time   = strrep(data.Time, 'T', ' ');
            Time   = strrep(Time, 'Z', '');
            offset = round(Lon(1) * 24/360);
            Time   = datenum(Time, 31) + offset/24; % convert to local Time from GMT
            
            % Convert time to decimal Hour of day
            [Y,M,D,h,m,s] = datevec(Time);
            Time          = (Time - datenum(Y,M,D)) *24; % remove year, month, day
            
            % calculate distance from lat lon and reference ellipsoid
            %%%%% something stopped working for 'distance'. it is
            %%%%% calculating distance and displacement the same. need to
            %%%%% fix. maybe matlab bug? 
            e = wgs84Ellipsoid;
            earthRadiusInMeters = 6371000;
%             Distance      = distance(Lat(2:end),Lon(2:end),Lat(1:end-1),Lon(1:end-1), e)'; % meters
            Distance      = earthdist(Lat(2:end),Lon(2:end),Lat(1:end-1),Lon(1:end-1), earthRadiusInMeters); % meters
%             Displacement  = distance(Lat(2:end),Lon(2:end),Lat(1),Lon(1), e)'; % meters
            Displacement  = earthdist(Lat(2:end),Lon(2:end),Lat(1),Lon(1), earthRadiusInMeters); % meters
            Elevation     = Elevation(2:end);
            
            % calculate time intervals and resulting speeds
            dT    = (Time(2:end)-Time(1:end-1))*60*60;       % s, time interval of samples
            Speed = Distance./dT;
            
            % store variables in a table (data frame)
            Time = Time(2:end);
            df   = table(Time, Distance, dT, Elevation, Speed, Displacement);
            
            % so i don't accidentally call uncleansed data:
            clear Distance Displacement Elevation Speed dT Lat Lon e data Time Y M D h m s
            
            % remove stand-still points %%%%%%this doesn't work while 'distance' isn't working
            df = df((df.Speed > .3),:);  % .2 m/s: 1.3-1.8 is ave walk, .2m/s is a 8 min 100 m swim
            
            % calculate total-activity variables
            Umean     = mean(df.Speed);                        % m/s, average speed
            Umed      = median(df.Speed);                      % m/s, median speed
            Umax      = max(df.Speed);                         % m/s, max speed
            Dtotal    = sum(df.Distance);                      % m, total distance covered
            dEmax     = max(df.Elevation) - min(df.Elevation); % m, elevation change
            Grademed  = median( abs( ( df.Elevation(2:end) - df.Elevation(1:end-1) )  ./( df.Distance(2:end) )));
            
            Tstart = df.Time(1);          % hrs, start time of activity
            Ttotal = sum(df.dT/60/60);    % hrs, sum of moving time intervals (not elapsed)
            
            Iswim  = double(Umed < SwimThresh); % binary, True if speed is less than physical swim limit
            Ibike  = double(Umed > RunThresh);  % binary, True if speed is more than physical run limit
            
            %% calculate 10-min bin stats (use entire activity if not greater than 10 minutes)
            % truncate so that there are constant size bins
            
            if size(df,1) > BinPoints
                df_bin = df(1:end-rem(size(df,1), BinPoints) , :);
                Nobs = size(df_bin,1);
                
                % Calculate relevant variables over bin period
                Displacement = reshape(df_bin.Displacement, BinPoints, Nobs/BinPoints);
                Displacement = Displacement - meshgrid(Displacement(1,:), 1:size(Displacement,1));
                Distance     = reshape(df_bin.Distance,     BinPoints, Nobs/BinPoints);
                Elevation    = reshape(df_bin.Elevation,    BinPoints, Nobs/BinPoints);
                
                Turns     = max(abs(Displacement))./sum(Distance(1:end-1,:));
                Turnsmed  = median(Turns);
                Turnsmin  = min(Turns);
                Turnsmean = mean(Turns);
                
                Dispmed   = median(max(abs(Displacement)));
                
                %             Grademed  = median(mean(abs((Elevation(2:end,:)-Elevation(1:end-1,:))./(Distance(2:end,:)))));
            else
                
                Turns     = max(abs(df.Displacement))./sum(df.Distance);
                Turnsmed  = median(Turns);
                Turnsmin  = min(Turns);
                Turnsmean = mean(Turns);
                
                Dispmed   = max(abs(df.Displacement));
                
                %             Grademed  = mean( abs( ( df.Elevation(2:end) - df.Elevation(1:end-1) )  ./( df.Distance(2:end) )));
            end
            
            activities(kk,:) = table(Umean, Umax, Umed, Tstart, Ttotal, Dtotal, Dispmed,...
                dEmax, Iswim, Ibike, Turnsmed, Turnsmin, Turnsmean,...
                Grademed, {Activity}, {Athlete}, {FileID});
            
        end
        writetable(activities, ['../../csv files/test',Athlete,ActivityType{jj},'.csv'])
        
    end 
end

% found using ('tic' and 'toc'):
% Adam's swims took 34 seconds to process (33 files)
% Adam's walks took 131 seconds (24 files)
% Adam's runs took ~1 hr
% Adam's rides took 2 hr 25 min


