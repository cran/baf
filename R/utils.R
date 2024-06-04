state_lookups <- data.frame(
  stringsAsFactors = FALSE,
  fips = c(
    '01', '02', '04', '05', '06',
    '08', '09', '10', '11', '12', '13', '15', '16', '17', '18',
    '19', '20', '21', '22', '23', '24', '25', '26', '27',
    '28', '29', '30', '31', '32', '33', '34', '35', '36', '37',
    '38', '39', '40', '41', '42', '44', '45', '46', '47',
    '48', '49', '50', '51', '53', '54', '55', '56', '60', '66',
    '69', '72', '74', '78'
  ),
  abb = c(
    'AL', 'AK', 'AZ', 'AR', 'CA',
    'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN',
    'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN',
    'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC',
    'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN',
    'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY', 'AS', 'GU',
    'MP', 'PR', 'UM', 'VI'
  ),
  name = c(
    'Alabama', 'Alaska', 'Arizona',
    'Arkansas', 'California', 'Colorado', 'Connecticut',
    'Delaware', 'District of Columbia', 'Florida', 'Georgia',
    'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas',
    'Kentucky', 'Louisiana', 'Maine', 'Maryland',
    'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri',
    'Montana', 'Nebraska', 'Nevada', 'New Hampshire',
    'New Jersey', 'New Mexico', 'New York', 'North Carolina',
    'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania',
    'Rhode Island', 'South Carolina', 'South Dakota',
    'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia',
    'Washington', 'West Virginia', 'Wisconsin', 'Wyoming',
    'American Samoa', 'Guam', 'Northern Mariana Islands', 'Puerto Rico',
    'U.S. Minor Outlying Islands', 'U.S. Virgin Islands'
  ),
  ansi = c(
    '1779775', '1785533', '1779777',
    '0068085', '1779778', '1779779', '1779780', '1779781',
    '1702382', '0294478', '1705317', '1779782', '1779783',
    '1779784', '0448508', '1779785', '0481813', '1779786',
    '1629543', '1779787', '1714934', '0606926', '1779789',
    '0662849', '1779790', '1779791', '0767982', '1779792',
    '1779793', '1779794', '1779795', '0897535', '1779796', '1027616',
    '1779797', '1085497', '1102857', '1155107', '1779798',
    '1219835', '1779799', '1785534', '1325873', '1779801',
    '1455989', '1779802', '1779803', '1779804', '1779805',
    '1779806', '1779807', '1802701', '1802705', '1779809',
    '1779808', '1878752', '1802710'
  )
)

get_state_matches <- function(state) {
  pos <- tolower(c(state_lookups$fips, state_lookups$abb, state_lookups$name, state_lookups$ansi))
  state <- tolower(state)
  matched <- match(state, pos)

  matched <- (matched %% nrow(state_lookups))
  ifelse(matched == 0, 57, matched)
}

lookup_state_fips <- function(state) {
  state_lookups$fips[get_state_matches(state)]
}

lookup_state_abb <- function(state) {
  state_lookups$abb[get_state_matches(state)]
}
