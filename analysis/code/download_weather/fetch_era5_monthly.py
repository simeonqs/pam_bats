import cdsapi
import os

#variables to download
era5_variable_array = ['total_precipitation', '2m_temperature', '10m_u_component_of_wind', 
'10m_v_component_of_wind', 'total_cloud_cover', 'surface_pressure']
variable_store_array = ['precip', 'temp', 'windu', 'windv', 'cloud_coverage', 'atm_pressure']

#path to store the data
store_path = '/home/au472091/OneDrive/au/projects/pam_bats/analysis/data/weather/'

if not os.path.exists(store_path):
    print('The path does not exist. Please check the path to store the data:', store_path)
    exit()

c = cdsapi.Client(url="https://cds.climate.copernicus.eu/api", key="fbcf6d2f-6dd5-47b9-9990-b0679803e7b1")
start_year = 2022
start_month = 11 # works with the start year
end_year = 2022 # included
end_month = 12 # included, works with the end year

#calculating the start and end year_month together
start_year_month = start_year*100 + start_month
end_year_month = end_year*100 + end_month

year_month = start_year_month
while year_month <= end_year_month:
    year = str(int(year_month/100))
    month = str(year_month%100).zfill(2)
    print (year, month)

    for variable_name, store_name in zip(era5_variable_array, variable_store_array):
        file_to_store = store_path + store_name + '/era5_' + store_name + '_' + year + '_' + month + '.grib'
        print(variable_name, file_to_store, ' downloading...')
        c.retrieve(
            'reanalysis-era5-single-levels',
            {
                'product_type': 'reanalysis',
                'format': 'grib',
                'year': year,
                'month': month,
                'day': [
                    '01', '02', '03',
                    '04', '05', '06',
                    '07', '08', '09',
                    '10', '11', '12',
                    '13', '14', '15',
                    '16', '17', '18',
                    '19', '20', '21',
                    '22', '23', '24',
                    '25', '26', '27',
                    '28', '29', '30',
                    '31',
                ],
                'time': [
                    '00:00', '01:00', '02:00',
                    '03:00', '04:00', '05:00',
                    '06:00', '07:00', '08:00',
                    '09:00', '10:00', '11:00',
                    '12:00', '13:00', '14:00',
                    '15:00', '16:00', '17:00',
                    '18:00', '19:00', '20:00',
                    '21:00', '22:00', '23:00',
                ],
                'variable': variable_name,
                'area': [
                    80, -30, 30,
                    50,
                ],
            },
            file_to_store)
    # Incrementing the year_month
    year_month += 1
    if(year_month%100 == 13):
        year_month += 88