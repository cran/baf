befs <- c(
  # 109 - 112 are identical baf
  cd_113 = 'https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2013/113-congressional-district-bef/cd113.zip',
  cd_114 = 'https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2015/114-congressional-district-bef/cd114.zip',
  cd_115 = 'https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2017/115-congressional-district-bef/cd115.zip',
  cd_116 = 'https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2019/116-congressional-district-bef/cd116.zip',
  # 2020 baf - 117
  cd_118 = 'https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2023/118-congressional-district-bef/cd118.zip',
  shd_2012 = 'https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2012/2012-state-legislative-bef/sldl_post2010.zip',
  ssd_2012 = 'https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2012/2012-state-legislative-bef/sldu_post2010.zip',
  shd_2016 = 'https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2016/2016-state-legislative-bef/sldl_2016.zip',
  ssd_2016 = 'https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2016/2016-state-legislative-bef/sldu_2016.zip',
  shd_2022 = 'https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2023/2022-state-legislative-bef/sldl_2022.zip',
  ssd_2022 = 'https://www2.census.gov/programs-surveys/decennial/rdo/mapping-files/2023/2022-state-legislative-bef/sldu_2022.zip'
)

bafs <- c(
  cd_109 = 'https://www2.census.gov/geo/docs/maps-data/data/baf/BlockAssign_ST{fips}_{abb}.zip',
  cd_117 = 'https://www2.census.gov/geo/docs/maps-data/data/baf2020/BlockAssign_ST{fips}_{abb}.zip'
)

baf_links <- dplyr::tribble(
  ~level, ~years, ~geography, ~decade, ~url, ~short_name,
  # congressional districts
  'state', c(2005:2012), 'cd', 2010, bafs['cd_109'], 'CD109',
  'national', c(2013:2014), 'cd', 2010, befs['cd_113'], 'CD113',
  'national', c(2015:2016), 'cd', 2010, befs['cd_114'], 'CD114',
  'national', c(2017:2018), 'cd', 2010, befs['cd_115'], 'CD115',
  'national', c(2019:2020), 'cd', 2010, befs['cd_116'], 'CD116',
  'state', c(2021:2022), 'cd', 2020, bafs['cd_117'], 'CD117',
  'national', c(2023:2024), 'cd', 2020, befs['cd_118'], 'CD118',

  # state legislative districts
  'national', c(2012:2015), 'shd', 2010, befs['shd_2012'], 'SHD2012',
  'national', c(2016:2021), 'shd', 2010, befs['shd_2016'], 'SHD2016',
  'national', c(2022:2024), 'shd', 2020, befs['shd_2022'], 'SHD2022',
  'national', c(2012:2015), 'ssd', 2010, befs['ssd_2012'], 'SSD2012',
  'national', c(2016:2021), 'ssd', 2010, befs['ssd_2016'], 'SSD2016',
  'national', c(2022:2024), 'ssd', 2020, befs['ssd_2022'], 'SSD2022',

  # general bafs
  'state', c(2010:2019), 'etc', 2010, bafs['cd_109'], NA_character_,
  'state', c(2020:2029), 'etc', 2020, bafs['cd_117'], NA_character_
)


lookup_links <- function(year, geographies) {
  if (is.null(geographies)) {
    geographies <- 'etc'
  }
  geographies <- tolower(geographies)
  if (!all(geographies %in% c('cd', 'shd', 'ssd', 'etc'))) {
    geographies[!geographies %in% c('cd', 'shd', 'ssd', 'etc')] <- 'etc'
  }

  geographies <- unique(geographies)

  baf_links |>
    dplyr::filter(
      vapply(.data$years, function(x) any(year %in% x), FUN.VALUE = logical(1)) &
        .data$geography %in% geographies
    )
}
