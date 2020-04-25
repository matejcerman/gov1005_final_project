# Define a list of allowed school levels and visualization indicators

reg_indicators <- c('Average monthly salary (gross)' = 'income',
                    'Unemployment rate' = 'unempl',
                    'Population density' = 'dens',
                    'Total population' = 'pop')

school_map_indicators <- c(
    'Overall rating (0-10 scale)' = 'overall',
    'Leaving examination preformance (0-10 scale)' = 'maturity',
    'Average leaving exam score: Slovak language (%)' = 'mat_sj',
    'Average leaving exam score: Math (%)' = 'mat_m',
    'Average leaving exam score: English language (%)' = 'mat_aj',
    'Average leaving exam score: Hungarian language  (%)' = 'mat_mj',
    'Average leaving exam score: Slovak as second language (%)' = 'mat_s_ja_sl',
    'Overall rating (0-10 scale)' = 'overall',
    'Grade 9 standardized testing performance (0-10 scale)' = 'testovanie9',
    'Grade 9 test score: Slovak Language (%)' = 't9_sj',
    'Grade 9 test score: Math (%)' = 't9_m',
    'Grade 9 test score: Hungarian Language (%)' = 't9_mj',
    'Grade 9 test score: Slovak as second language (%)' = 't9_s_ja_sl'
)

school_gen_indicators <- c(
  'Overall rating (0-10 scale)' = 'overall_rating',
  'Leaving examination preformance (0-10 scale)' = 'maturity',
  'Average leaving exam score: Slovak language (%)' = 'mat_sj',
  'Average leaving exam score: Math (%)' = 'mat_m',
  'Average leaving exam score: English language (%)' = 'mat_aj',
  'Average leaving exam score: Hungarian language  (%)' = 'mat_mj',
  'Average leaving exam score: Slovak as second language (%)' = 'mat_s_ja_sl',
  'Teachers per 100 students' = 'teachers',
  'Overall rating (0-10 scale)' = 'overall_rating',
  'Grade 9 standardized testing performance (0-10 scale)' = 'testovanie9',
  'Grade 9 test score: Slovak Language (%)' = 't9_sj',
  'Grade 9 test score: Math (%)' = 't9_m',
  'Grade 9 test score: Hungarian Language (%)' = 't9_mj',
  'Grade 9 test score: Slovak as second language (%)' = 't9_s_ja_sl',
  'Teachers per 100 students' = 'teachers'
)