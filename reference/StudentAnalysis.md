# StudentAnalysis

The StudentAnalysis function returns descriptive statistics for each
individual student. Specifically, it provides the number of responses,
the number of correct answers, the passage rate, the standardized score,
the percentile, and the stanine.

## Usage

``` r
StudentAnalysis(U, na = NULL, Z = NULL, w = NULL)
```

## Arguments

- U:

  U is a data matrix of the type matrix or data.frame.

- na:

  na argument specifies the numbers or characters to be treated as
  missing values.

  - ID: Student identifier

  - NR: Number of responses

  - NRS: Number-right score (total correct answers)

  - PR: Passage rate (proportion correct)

  - SS: Standardized score (z-score)

  - Percentile: Student's percentile rank

  - Stanine: Student's stanine score (1-9)

- Z:

  Z is a missing indicator matrix of the type matrix or data.frame

- w:

  w is item weight vector

## Value

Returns a data frame containing the following columns for each student:

- ID: Student identifier

- NR: Number of responses

- NRS: Number-right score (total correct answers)

- PR: Passage rate (proportion correct)

- SS: Standardized score (z-score)

- Percentile: Student's percentile rank

- Stanine: Student's stanine score (1-9)

## Examples

``` r
# using sample dataset
StudentAnalysis(J15S500)
#>                    ID NR NRS        PR         SS Percentile Stanine
#> Student001 Student001 15   8 0.5333333 -0.6249599         32       4
#> Student002 Student002 15  10 0.6666667  0.1261938         57       5
#> Student003 Student003 15  10 0.6666667  0.1261938         57       5
#> Student004 Student004 15  11 0.7333333  0.5017707         74       6
#> Student005 Student005 15   6 0.4000000 -1.3761137         15       3
#> Student006 Student006 15  12 0.8000000  0.8773476         87       7
#> Student007 Student007 15   9 0.6000000 -0.2493831         42       5
#> Student008 Student008 15  10 0.6666667  0.1261938         57       5
#> Student009 Student009 15  14 0.9333333  1.6285014         99       9
#> Student010 Student010 15   9 0.6000000 -0.2493831         42       5
#> Student011 Student011 15   9 0.6000000 -0.2493831         42       5
#> Student012 Student012 15   8 0.5333333 -0.6249599         32       4
#> Student013 Student013 15  13 0.8666667  1.2529245         96       8
#> Student014 Student014 15  12 0.8000000  0.8773476         87       7
#> Student015 Student015 15   5 0.3333333 -1.7516906          8       2
#> Student016 Student016 15   5 0.3333333 -1.7516906          8       2
#> Student017 Student017 15  12 0.8000000  0.8773476         87       7
#> Student018 Student018 15   7 0.4666667 -1.0005368         24       4
#> Student019 Student019 15  12 0.8000000  0.8773476         87       7
#> Student020 Student020 15   6 0.4000000 -1.3761137         15       3
#> Student021 Student021 15  13 0.8666667  1.2529245         96       8
#> Student022 Student022 15   8 0.5333333 -0.6249599         32       4
#> Student023 Student023 15  10 0.6666667  0.1261938         57       5
#> Student024 Student024 15  10 0.6666667  0.1261938         57       5
#> Student025 Student025 15  10 0.6666667  0.1261938         57       5
#> Student026 Student026 15   5 0.3333333 -1.7516906          8       2
#> Student027 Student027 15   9 0.6000000 -0.2493831         42       5
#> Student028 Student028 15  11 0.7333333  0.5017707         74       6
#> Student029 Student029 15  11 0.7333333  0.5017707         74       6
#> Student030 Student030 15   4 0.2666667 -2.1272675          3       1
#> Student031 Student031 15   5 0.3333333 -1.7516906          8       2
#> Student032 Student032 15  11 0.7333333  0.5017707         74       6
#> Student033 Student033 15   5 0.3333333 -1.7516906          8       2
#> Student034 Student034 15   5 0.3333333 -1.7516906          8       2
#> Student035 Student035 15  15 1.0000000  2.0040783        100       9
#> Student036 Student036 15   8 0.5333333 -0.6249599         32       4
#> Student037 Student037 15  12 0.8000000  0.8773476         87       7
#> Student038 Student038 15  11 0.7333333  0.5017707         74       6
#> Student039 Student039 15   8 0.5333333 -0.6249599         32       4
#> Student040 Student040 15  10 0.6666667  0.1261938         57       5
#> Student041 Student041 15  11 0.7333333  0.5017707         74       6
#> Student042 Student042 15  14 0.9333333  1.6285014         99       9
#> Student043 Student043 15  10 0.6666667  0.1261938         57       5
#> Student044 Student044 15   9 0.6000000 -0.2493831         42       5
#> Student045 Student045 15  12 0.8000000  0.8773476         87       7
#> Student046 Student046 15  11 0.7333333  0.5017707         74       6
#> Student047 Student047 15  10 0.6666667  0.1261938         57       5
#> Student048 Student048 15  12 0.8000000  0.8773476         87       7
#> Student049 Student049 15  11 0.7333333  0.5017707         74       6
#> Student050 Student050 15  13 0.8666667  1.2529245         96       8
#> Student051 Student051 15  11 0.7333333  0.5017707         74       6
#> Student052 Student052 15  12 0.8000000  0.8773476         87       7
#> Student053 Student053 15  10 0.6666667  0.1261938         57       5
#> Student054 Student054 15  11 0.7333333  0.5017707         74       6
#> Student055 Student055 15  13 0.8666667  1.2529245         96       8
#> Student056 Student056 15  12 0.8000000  0.8773476         87       7
#> Student057 Student057 15   9 0.6000000 -0.2493831         42       5
#> Student058 Student058 15  11 0.7333333  0.5017707         74       6
#> Student059 Student059 15  14 0.9333333  1.6285014         99       9
#> Student060 Student060 15  12 0.8000000  0.8773476         87       7
#> Student061 Student061 15  14 0.9333333  1.6285014         99       9
#> Student062 Student062 15  11 0.7333333  0.5017707         74       6
#> Student063 Student063 15   9 0.6000000 -0.2493831         42       5
#> Student064 Student064 15  11 0.7333333  0.5017707         74       6
#> Student065 Student065 15  10 0.6666667  0.1261938         57       5
#> Student066 Student066 15   5 0.3333333 -1.7516906          8       2
#> Student067 Student067 15   7 0.4666667 -1.0005368         24       4
#> Student068 Student068 15   6 0.4000000 -1.3761137         15       3
#> Student069 Student069 15  12 0.8000000  0.8773476         87       7
#> Student070 Student070 15   6 0.4000000 -1.3761137         15       3
#> Student071 Student071 15   5 0.3333333 -1.7516906          8       2
#> Student072 Student072 15  10 0.6666667  0.1261938         57       5
#> Student073 Student073 15  10 0.6666667  0.1261938         57       5
#> Student074 Student074 15  10 0.6666667  0.1261938         57       5
#> Student075 Student075 15  12 0.8000000  0.8773476         87       7
#> Student076 Student076 15   8 0.5333333 -0.6249599         32       4
#> Student077 Student077 15  13 0.8666667  1.2529245         96       8
#> Student078 Student078 15  12 0.8000000  0.8773476         87       7
#> Student079 Student079 15  12 0.8000000  0.8773476         87       7
#> Student080 Student080 15   4 0.2666667 -2.1272675          3       1
#> Student081 Student081 15   9 0.6000000 -0.2493831         42       5
#> Student082 Student082 15   5 0.3333333 -1.7516906          8       2
#> Student083 Student083 15   8 0.5333333 -0.6249599         32       4
#> Student084 Student084 15   8 0.5333333 -0.6249599         32       4
#> Student085 Student085 15  12 0.8000000  0.8773476         87       7
#> Student086 Student086 15   8 0.5333333 -0.6249599         32       4
#> Student087 Student087 15  11 0.7333333  0.5017707         74       6
#> Student088 Student088 15   8 0.5333333 -0.6249599         32       4
#> Student089 Student089 15  12 0.8000000  0.8773476         87       7
#> Student090 Student090 15  11 0.7333333  0.5017707         74       6
#> Student091 Student091 15   7 0.4666667 -1.0005368         24       4
#> Student092 Student092 15   6 0.4000000 -1.3761137         15       3
#> Student093 Student093 15   8 0.5333333 -0.6249599         32       4
#> Student094 Student094 15  12 0.8000000  0.8773476         87       7
#> Student095 Student095 15   8 0.5333333 -0.6249599         32       4
#> Student096 Student096 15  11 0.7333333  0.5017707         74       6
#> Student097 Student097 15  10 0.6666667  0.1261938         57       5
#> Student098 Student098 15   6 0.4000000 -1.3761137         15       3
#> Student099 Student099 15  11 0.7333333  0.5017707         74       6
#> Student100 Student100 15  12 0.8000000  0.8773476         87       7
#> Student101 Student101 15  10 0.6666667  0.1261938         57       5
#> Student102 Student102 15   6 0.4000000 -1.3761137         15       3
#> Student103 Student103 15  13 0.8666667  1.2529245         96       8
#> Student104 Student104 15   5 0.3333333 -1.7516906          8       2
#> Student105 Student105 15  11 0.7333333  0.5017707         74       6
#> Student106 Student106 15   7 0.4666667 -1.0005368         24       4
#> Student107 Student107 15  11 0.7333333  0.5017707         74       6
#> Student108 Student108 15  11 0.7333333  0.5017707         74       6
#> Student109 Student109 15   6 0.4000000 -1.3761137         15       3
#> Student110 Student110 15  12 0.8000000  0.8773476         87       7
#> Student111 Student111 15  12 0.8000000  0.8773476         87       7
#> Student112 Student112 15  11 0.7333333  0.5017707         74       6
#> Student113 Student113 15   6 0.4000000 -1.3761137         15       3
#> Student114 Student114 15   8 0.5333333 -0.6249599         32       4
#> Student115 Student115 15   5 0.3333333 -1.7516906          8       2
#> Student116 Student116 15  13 0.8666667  1.2529245         96       8
#> Student117 Student117 15  11 0.7333333  0.5017707         74       6
#> Student118 Student118 15   7 0.4666667 -1.0005368         24       4
#> Student119 Student119 15   8 0.5333333 -0.6249599         32       4
#> Student120 Student120 15   8 0.5333333 -0.6249599         32       4
#> Student121 Student121 15  10 0.6666667  0.1261938         57       5
#> Student122 Student122 15   8 0.5333333 -0.6249599         32       4
#> Student123 Student123 15  12 0.8000000  0.8773476         87       7
#> Student124 Student124 15  13 0.8666667  1.2529245         96       8
#> Student125 Student125 15   9 0.6000000 -0.2493831         42       5
#> Student126 Student126 15  13 0.8666667  1.2529245         96       8
#> Student127 Student127 15   4 0.2666667 -2.1272675          3       1
#> Student128 Student128 15  11 0.7333333  0.5017707         74       6
#> Student129 Student129 15  12 0.8000000  0.8773476         87       7
#> Student130 Student130 15  12 0.8000000  0.8773476         87       7
#> Student131 Student131 15  10 0.6666667  0.1261938         57       5
#> Student132 Student132 15  10 0.6666667  0.1261938         57       5
#> Student133 Student133 15  11 0.7333333  0.5017707         74       6
#> Student134 Student134 15   5 0.3333333 -1.7516906          8       2
#> Student135 Student135 15   8 0.5333333 -0.6249599         32       4
#> Student136 Student136 15   5 0.3333333 -1.7516906          8       2
#> Student137 Student137 15  13 0.8666667  1.2529245         96       8
#> Student138 Student138 15  11 0.7333333  0.5017707         74       6
#> Student139 Student139 15  11 0.7333333  0.5017707         74       6
#> Student140 Student140 15  10 0.6666667  0.1261938         57       5
#> Student141 Student141 15  11 0.7333333  0.5017707         74       6
#> Student142 Student142 15   5 0.3333333 -1.7516906          8       2
#> Student143 Student143 15  13 0.8666667  1.2529245         96       8
#> Student144 Student144 15   4 0.2666667 -2.1272675          3       1
#> Student145 Student145 15  12 0.8000000  0.8773476         87       7
#> Student146 Student146 15   7 0.4666667 -1.0005368         24       4
#> Student147 Student147 15   5 0.3333333 -1.7516906          8       2
#> Student148 Student148 15  12 0.8000000  0.8773476         87       7
#> Student149 Student149 15   9 0.6000000 -0.2493831         42       5
#> Student150 Student150 15  13 0.8666667  1.2529245         96       8
#> Student151 Student151 15  13 0.8666667  1.2529245         96       8
#> Student152 Student152 15  11 0.7333333  0.5017707         74       6
#> Student153 Student153 15  14 0.9333333  1.6285014         99       9
#> Student154 Student154 15   9 0.6000000 -0.2493831         42       5
#> Student155 Student155 15   6 0.4000000 -1.3761137         15       3
#> Student156 Student156 15  11 0.7333333  0.5017707         74       6
#> Student157 Student157 15  10 0.6666667  0.1261938         57       5
#> Student158 Student158 15  13 0.8666667  1.2529245         96       8
#> Student159 Student159 15  13 0.8666667  1.2529245         96       8
#> Student160 Student160 15  15 1.0000000  2.0040783        100       9
#> Student161 Student161 15  11 0.7333333  0.5017707         74       6
#> Student162 Student162 15   7 0.4666667 -1.0005368         24       4
#> Student163 Student163 15   7 0.4666667 -1.0005368         24       4
#> Student164 Student164 15   5 0.3333333 -1.7516906          8       2
#> Student165 Student165 15  10 0.6666667  0.1261938         57       5
#> Student166 Student166 15   5 0.3333333 -1.7516906          8       2
#> Student167 Student167 15  10 0.6666667  0.1261938         57       5
#> Student168 Student168 15   8 0.5333333 -0.6249599         32       4
#> Student169 Student169 15  14 0.9333333  1.6285014         99       9
#> Student170 Student170 15   6 0.4000000 -1.3761137         15       3
#> Student171 Student171 15   6 0.4000000 -1.3761137         15       3
#> Student172 Student172 15  13 0.8666667  1.2529245         96       8
#> Student173 Student173 15  10 0.6666667  0.1261938         57       5
#> Student174 Student174 15  12 0.8000000  0.8773476         87       7
#> Student175 Student175 15   7 0.4666667 -1.0005368         24       4
#> Student176 Student176 15  10 0.6666667  0.1261938         57       5
#> Student177 Student177 15  11 0.7333333  0.5017707         74       6
#> Student178 Student178 15   8 0.5333333 -0.6249599         32       4
#> Student179 Student179 15  10 0.6666667  0.1261938         57       5
#> Student180 Student180 15   8 0.5333333 -0.6249599         32       4
#> Student181 Student181 15   7 0.4666667 -1.0005368         24       4
#> Student182 Student182 15   6 0.4000000 -1.3761137         15       3
#> Student183 Student183 15  11 0.7333333  0.5017707         74       6
#> Student184 Student184 15   8 0.5333333 -0.6249599         32       4
#> Student185 Student185 15  11 0.7333333  0.5017707         74       6
#> Student186 Student186 15  11 0.7333333  0.5017707         74       6
#> Student187 Student187 15   8 0.5333333 -0.6249599         32       4
#> Student188 Student188 15  11 0.7333333  0.5017707         74       6
#> Student189 Student189 15   9 0.6000000 -0.2493831         42       5
#> Student190 Student190 15  10 0.6666667  0.1261938         57       5
#> Student191 Student191 15  10 0.6666667  0.1261938         57       5
#> Student192 Student192 15   7 0.4666667 -1.0005368         24       4
#> Student193 Student193 15  10 0.6666667  0.1261938         57       5
#> Student194 Student194 15   6 0.4000000 -1.3761137         15       3
#> Student195 Student195 15   3 0.2000000 -2.5028444          1       1
#> Student196 Student196 15  12 0.8000000  0.8773476         87       7
#> Student197 Student197 15  11 0.7333333  0.5017707         74       6
#> Student198 Student198 15  13 0.8666667  1.2529245         96       8
#> Student199 Student199 15  10 0.6666667  0.1261938         57       5
#> Student200 Student200 15  11 0.7333333  0.5017707         74       6
#> Student201 Student201 15  12 0.8000000  0.8773476         87       7
#> Student202 Student202 15  11 0.7333333  0.5017707         74       6
#> Student203 Student203 15   5 0.3333333 -1.7516906          8       2
#> Student204 Student204 15  12 0.8000000  0.8773476         87       7
#> Student205 Student205 15  10 0.6666667  0.1261938         57       5
#> Student206 Student206 15   8 0.5333333 -0.6249599         32       4
#> Student207 Student207 15  12 0.8000000  0.8773476         87       7
#> Student208 Student208 15  11 0.7333333  0.5017707         74       6
#> Student209 Student209 15   9 0.6000000 -0.2493831         42       5
#> Student210 Student210 15  11 0.7333333  0.5017707         74       6
#> Student211 Student211 15  10 0.6666667  0.1261938         57       5
#> Student212 Student212 15  14 0.9333333  1.6285014         99       9
#> Student213 Student213 15  10 0.6666667  0.1261938         57       5
#> Student214 Student214 15   8 0.5333333 -0.6249599         32       4
#> Student215 Student215 15  10 0.6666667  0.1261938         57       5
#> Student216 Student216 15   9 0.6000000 -0.2493831         42       5
#> Student217 Student217 15  13 0.8666667  1.2529245         96       8
#> Student218 Student218 15  12 0.8000000  0.8773476         87       7
#> Student219 Student219 15   4 0.2666667 -2.1272675          3       1
#> Student220 Student220 15   6 0.4000000 -1.3761137         15       3
#> Student221 Student221 15   5 0.3333333 -1.7516906          8       2
#> Student222 Student222 15  11 0.7333333  0.5017707         74       6
#> Student223 Student223 15   9 0.6000000 -0.2493831         42       5
#> Student224 Student224 15   9 0.6000000 -0.2493831         42       5
#> Student225 Student225 15  10 0.6666667  0.1261938         57       5
#> Student226 Student226 15  10 0.6666667  0.1261938         57       5
#> Student227 Student227 15   9 0.6000000 -0.2493831         42       5
#> Student228 Student228 15  11 0.7333333  0.5017707         74       6
#> Student229 Student229 15   7 0.4666667 -1.0005368         24       4
#> Student230 Student230 15  10 0.6666667  0.1261938         57       5
#> Student231 Student231 15   7 0.4666667 -1.0005368         24       4
#> Student232 Student232 15   4 0.2666667 -2.1272675          3       1
#> Student233 Student233 15  12 0.8000000  0.8773476         87       7
#> Student234 Student234 15   4 0.2666667 -2.1272675          3       1
#> Student235 Student235 15   9 0.6000000 -0.2493831         42       5
#> Student236 Student236 15   9 0.6000000 -0.2493831         42       5
#> Student237 Student237 15   7 0.4666667 -1.0005368         24       4
#> Student238 Student238 15  14 0.9333333  1.6285014         99       9
#> Student239 Student239 15  12 0.8000000  0.8773476         87       7
#> Student240 Student240 15  11 0.7333333  0.5017707         74       6
#> Student241 Student241 15  12 0.8000000  0.8773476         87       7
#> Student242 Student242 15  11 0.7333333  0.5017707         74       6
#> Student243 Student243 15   9 0.6000000 -0.2493831         42       5
#> Student244 Student244 15   8 0.5333333 -0.6249599         32       4
#> Student245 Student245 15  11 0.7333333  0.5017707         74       6
#> Student246 Student246 15   5 0.3333333 -1.7516906          8       2
#> Student247 Student247 15  11 0.7333333  0.5017707         74       6
#> Student248 Student248 15  15 1.0000000  2.0040783        100       9
#> Student249 Student249 15   8 0.5333333 -0.6249599         32       4
#> Student250 Student250 15  14 0.9333333  1.6285014         99       9
#> Student251 Student251 15   9 0.6000000 -0.2493831         42       5
#> Student252 Student252 15   7 0.4666667 -1.0005368         24       4
#> Student253 Student253 15  11 0.7333333  0.5017707         74       6
#> Student254 Student254 15  10 0.6666667  0.1261938         57       5
#> Student255 Student255 15  11 0.7333333  0.5017707         74       6
#> Student256 Student256 15  10 0.6666667  0.1261938         57       5
#> Student257 Student257 15   8 0.5333333 -0.6249599         32       4
#> Student258 Student258 15   7 0.4666667 -1.0005368         24       4
#> Student259 Student259 15   7 0.4666667 -1.0005368         24       4
#> Student260 Student260 15   9 0.6000000 -0.2493831         42       5
#> Student261 Student261 15   7 0.4666667 -1.0005368         24       4
#> Student262 Student262 15  13 0.8666667  1.2529245         96       8
#> Student263 Student263 15  12 0.8000000  0.8773476         87       7
#> Student264 Student264 15   8 0.5333333 -0.6249599         32       4
#> Student265 Student265 15  12 0.8000000  0.8773476         87       7
#> Student266 Student266 15  11 0.7333333  0.5017707         74       6
#> Student267 Student267 15  11 0.7333333  0.5017707         74       6
#> Student268 Student268 15   8 0.5333333 -0.6249599         32       4
#> Student269 Student269 15  12 0.8000000  0.8773476         87       7
#> Student270 Student270 15   8 0.5333333 -0.6249599         32       4
#> Student271 Student271 15  12 0.8000000  0.8773476         87       7
#> Student272 Student272 15   9 0.6000000 -0.2493831         42       5
#> Student273 Student273 15   6 0.4000000 -1.3761137         15       3
#> Student274 Student274 15   9 0.6000000 -0.2493831         42       5
#> Student275 Student275 15  10 0.6666667  0.1261938         57       5
#> Student276 Student276 15  11 0.7333333  0.5017707         74       6
#> Student277 Student277 15   7 0.4666667 -1.0005368         24       4
#> Student278 Student278 15   6 0.4000000 -1.3761137         15       3
#> Student279 Student279 15  10 0.6666667  0.1261938         57       5
#> Student280 Student280 15   7 0.4666667 -1.0005368         24       4
#> Student281 Student281 15  10 0.6666667  0.1261938         57       5
#> Student282 Student282 15  12 0.8000000  0.8773476         87       7
#> Student283 Student283 15   7 0.4666667 -1.0005368         24       4
#> Student284 Student284 15  12 0.8000000  0.8773476         87       7
#> Student285 Student285 15   9 0.6000000 -0.2493831         42       5
#> Student286 Student286 15   6 0.4000000 -1.3761137         15       3
#> Student287 Student287 15  13 0.8666667  1.2529245         96       8
#> Student288 Student288 15   9 0.6000000 -0.2493831         42       5
#> Student289 Student289 15  13 0.8666667  1.2529245         96       8
#> Student290 Student290 15  15 1.0000000  2.0040783        100       9
#> Student291 Student291 15  10 0.6666667  0.1261938         57       5
#> Student292 Student292 15  14 0.9333333  1.6285014         99       9
#> Student293 Student293 15   4 0.2666667 -2.1272675          3       1
#> Student294 Student294 15  12 0.8000000  0.8773476         87       7
#> Student295 Student295 15   6 0.4000000 -1.3761137         15       3
#> Student296 Student296 15   7 0.4666667 -1.0005368         24       4
#> Student297 Student297 15  11 0.7333333  0.5017707         74       6
#> Student298 Student298 15  12 0.8000000  0.8773476         87       7
#> Student299 Student299 15   5 0.3333333 -1.7516906          8       2
#> Student300 Student300 15  11 0.7333333  0.5017707         74       6
#> Student301 Student301 15   7 0.4666667 -1.0005368         24       4
#> Student302 Student302 15  13 0.8666667  1.2529245         96       8
#> Student303 Student303 15   9 0.6000000 -0.2493831         42       5
#> Student304 Student304 15   9 0.6000000 -0.2493831         42       5
#> Student305 Student305 15  10 0.6666667  0.1261938         57       5
#> Student306 Student306 15  12 0.8000000  0.8773476         87       7
#> Student307 Student307 15   6 0.4000000 -1.3761137         15       3
#> Student308 Student308 15  12 0.8000000  0.8773476         87       7
#> Student309 Student309 15  11 0.7333333  0.5017707         74       6
#> Student310 Student310 15   9 0.6000000 -0.2493831         42       5
#> Student311 Student311 15  10 0.6666667  0.1261938         57       5
#> Student312 Student312 15  11 0.7333333  0.5017707         74       6
#> Student313 Student313 15  12 0.8000000  0.8773476         87       7
#> Student314 Student314 15   7 0.4666667 -1.0005368         24       4
#> Student315 Student315 15  11 0.7333333  0.5017707         74       6
#> Student316 Student316 15  11 0.7333333  0.5017707         74       6
#> Student317 Student317 15  13 0.8666667  1.2529245         96       8
#> Student318 Student318 15   7 0.4666667 -1.0005368         24       4
#> Student319 Student319 15   6 0.4000000 -1.3761137         15       3
#> Student320 Student320 15   9 0.6000000 -0.2493831         42       5
#> Student321 Student321 15   9 0.6000000 -0.2493831         42       5
#> Student322 Student322 15  13 0.8666667  1.2529245         96       8
#> Student323 Student323 15   9 0.6000000 -0.2493831         42       5
#> Student324 Student324 15  13 0.8666667  1.2529245         96       8
#> Student325 Student325 15  13 0.8666667  1.2529245         96       8
#> Student326 Student326 15  13 0.8666667  1.2529245         96       8
#> Student327 Student327 15   8 0.5333333 -0.6249599         32       4
#> Student328 Student328 15  10 0.6666667  0.1261938         57       5
#> Student329 Student329 15  11 0.7333333  0.5017707         74       6
#> Student330 Student330 15  10 0.6666667  0.1261938         57       5
#> Student331 Student331 15  12 0.8000000  0.8773476         87       7
#> Student332 Student332 15   7 0.4666667 -1.0005368         24       4
#> Student333 Student333 15   9 0.6000000 -0.2493831         42       5
#> Student334 Student334 15  13 0.8666667  1.2529245         96       8
#> Student335 Student335 15  11 0.7333333  0.5017707         74       6
#> Student336 Student336 15  10 0.6666667  0.1261938         57       5
#> Student337 Student337 15  10 0.6666667  0.1261938         57       5
#> Student338 Student338 15  12 0.8000000  0.8773476         87       7
#> Student339 Student339 15  13 0.8666667  1.2529245         96       8
#> Student340 Student340 15   9 0.6000000 -0.2493831         42       5
#> Student341 Student341 15   4 0.2666667 -2.1272675          3       1
#> Student342 Student342 15  10 0.6666667  0.1261938         57       5
#> Student343 Student343 15  10 0.6666667  0.1261938         57       5
#> Student344 Student344 15  14 0.9333333  1.6285014         99       9
#> Student345 Student345 15   7 0.4666667 -1.0005368         24       4
#> Student346 Student346 15  11 0.7333333  0.5017707         74       6
#> Student347 Student347 15  10 0.6666667  0.1261938         57       5
#> Student348 Student348 15  12 0.8000000  0.8773476         87       7
#> Student349 Student349 15  10 0.6666667  0.1261938         57       5
#> Student350 Student350 15  12 0.8000000  0.8773476         87       7
#> Student351 Student351 15  10 0.6666667  0.1261938         57       5
#> Student352 Student352 15   8 0.5333333 -0.6249599         32       4
#> Student353 Student353 15  11 0.7333333  0.5017707         74       6
#> Student354 Student354 15   8 0.5333333 -0.6249599         32       4
#> Student355 Student355 15  12 0.8000000  0.8773476         87       7
#> Student356 Student356 15  13 0.8666667  1.2529245         96       8
#> Student357 Student357 15   7 0.4666667 -1.0005368         24       4
#> Student358 Student358 15  14 0.9333333  1.6285014         99       9
#> Student359 Student359 15   2 0.1333333 -2.8784213          1       1
#> Student360 Student360 15   9 0.6000000 -0.2493831         42       5
#> Student361 Student361 15  12 0.8000000  0.8773476         87       7
#> Student362 Student362 15  13 0.8666667  1.2529245         96       8
#> Student363 Student363 15   7 0.4666667 -1.0005368         24       4
#> Student364 Student364 15  11 0.7333333  0.5017707         74       6
#> Student365 Student365 15  10 0.6666667  0.1261938         57       5
#> Student366 Student366 15  11 0.7333333  0.5017707         74       6
#> Student367 Student367 15  15 1.0000000  2.0040783        100       9
#> Student368 Student368 15   6 0.4000000 -1.3761137         15       3
#> Student369 Student369 15  10 0.6666667  0.1261938         57       5
#> Student370 Student370 15  10 0.6666667  0.1261938         57       5
#> Student371 Student371 15  14 0.9333333  1.6285014         99       9
#> Student372 Student372 15  11 0.7333333  0.5017707         74       6
#> Student373 Student373 15  13 0.8666667  1.2529245         96       8
#> Student374 Student374 15   9 0.6000000 -0.2493831         42       5
#> Student375 Student375 15   9 0.6000000 -0.2493831         42       5
#> Student376 Student376 15  10 0.6666667  0.1261938         57       5
#> Student377 Student377 15  10 0.6666667  0.1261938         57       5
#> Student378 Student378 15  10 0.6666667  0.1261938         57       5
#> Student379 Student379 15  14 0.9333333  1.6285014         99       9
#> Student380 Student380 15  12 0.8000000  0.8773476         87       7
#> Student381 Student381 15   2 0.1333333 -2.8784213          1       1
#> Student382 Student382 15   7 0.4666667 -1.0005368         24       4
#> Student383 Student383 15   7 0.4666667 -1.0005368         24       4
#> Student384 Student384 15   7 0.4666667 -1.0005368         24       4
#> Student385 Student385 15  12 0.8000000  0.8773476         87       7
#> Student386 Student386 15   8 0.5333333 -0.6249599         32       4
#> Student387 Student387 15   8 0.5333333 -0.6249599         32       4
#> Student388 Student388 15   7 0.4666667 -1.0005368         24       4
#> Student389 Student389 15  12 0.8000000  0.8773476         87       7
#> Student390 Student390 15   9 0.6000000 -0.2493831         42       5
#> Student391 Student391 15   6 0.4000000 -1.3761137         15       3
#> Student392 Student392 15   7 0.4666667 -1.0005368         24       4
#> Student393 Student393 15  11 0.7333333  0.5017707         74       6
#> Student394 Student394 15   7 0.4666667 -1.0005368         24       4
#> Student395 Student395 15  11 0.7333333  0.5017707         74       6
#> Student396 Student396 15   8 0.5333333 -0.6249599         32       4
#> Student397 Student397 15  11 0.7333333  0.5017707         74       6
#> Student398 Student398 15  12 0.8000000  0.8773476         87       7
#> Student399 Student399 15   6 0.4000000 -1.3761137         15       3
#> Student400 Student400 15   5 0.3333333 -1.7516906          8       2
#> Student401 Student401 15  13 0.8666667  1.2529245         96       8
#> Student402 Student402 15   7 0.4666667 -1.0005368         24       4
#> Student403 Student403 15  10 0.6666667  0.1261938         57       5
#> Student404 Student404 15  12 0.8000000  0.8773476         87       7
#> Student405 Student405 15  10 0.6666667  0.1261938         57       5
#> Student406 Student406 15  10 0.6666667  0.1261938         57       5
#> Student407 Student407 15   2 0.1333333 -2.8784213          1       1
#> Student408 Student408 15  11 0.7333333  0.5017707         74       6
#> Student409 Student409 15  10 0.6666667  0.1261938         57       5
#> Student410 Student410 15  12 0.8000000  0.8773476         87       7
#> Student411 Student411 15  12 0.8000000  0.8773476         87       7
#> Student412 Student412 15  10 0.6666667  0.1261938         57       5
#> Student413 Student413 15  10 0.6666667  0.1261938         57       5
#> Student414 Student414 15   9 0.6000000 -0.2493831         42       5
#> Student415 Student415 15  10 0.6666667  0.1261938         57       5
#> Student416 Student416 15  15 1.0000000  2.0040783        100       9
#> Student417 Student417 15   9 0.6000000 -0.2493831         42       5
#> Student418 Student418 15  13 0.8666667  1.2529245         96       8
#> Student419 Student419 15   6 0.4000000 -1.3761137         15       3
#> Student420 Student420 15   9 0.6000000 -0.2493831         42       5
#> Student421 Student421 15   9 0.6000000 -0.2493831         42       5
#> Student422 Student422 15   7 0.4666667 -1.0005368         24       4
#> Student423 Student423 15   9 0.6000000 -0.2493831         42       5
#> Student424 Student424 15  13 0.8666667  1.2529245         96       8
#> Student425 Student425 15   9 0.6000000 -0.2493831         42       5
#> Student426 Student426 15   9 0.6000000 -0.2493831         42       5
#> Student427 Student427 15  13 0.8666667  1.2529245         96       8
#> Student428 Student428 15   5 0.3333333 -1.7516906          8       2
#> Student429 Student429 15  11 0.7333333  0.5017707         74       6
#> Student430 Student430 15  11 0.7333333  0.5017707         74       6
#> Student431 Student431 15  10 0.6666667  0.1261938         57       5
#> Student432 Student432 15  12 0.8000000  0.8773476         87       7
#> Student433 Student433 15  11 0.7333333  0.5017707         74       6
#> Student434 Student434 15  13 0.8666667  1.2529245         96       8
#> Student435 Student435 15  10 0.6666667  0.1261938         57       5
#> Student436 Student436 15  14 0.9333333  1.6285014         99       9
#> Student437 Student437 15  11 0.7333333  0.5017707         74       6
#> Student438 Student438 15  10 0.6666667  0.1261938         57       5
#> Student439 Student439 15   8 0.5333333 -0.6249599         32       4
#> Student440 Student440 15  11 0.7333333  0.5017707         74       6
#> Student441 Student441 15  11 0.7333333  0.5017707         74       6
#> Student442 Student442 15  10 0.6666667  0.1261938         57       5
#> Student443 Student443 15  11 0.7333333  0.5017707         74       6
#> Student444 Student444 15  11 0.7333333  0.5017707         74       6
#> Student445 Student445 15   8 0.5333333 -0.6249599         32       4
#> Student446 Student446 15   6 0.4000000 -1.3761137         15       3
#> Student447 Student447 15  14 0.9333333  1.6285014         99       9
#> Student448 Student448 15  12 0.8000000  0.8773476         87       7
#> Student449 Student449 15  10 0.6666667  0.1261938         57       5
#> Student450 Student450 15  13 0.8666667  1.2529245         96       8
#> Student451 Student451 15  10 0.6666667  0.1261938         57       5
#> Student452 Student452 15  13 0.8666667  1.2529245         96       8
#> Student453 Student453 15   8 0.5333333 -0.6249599         32       4
#> Student454 Student454 15  12 0.8000000  0.8773476         87       7
#> Student455 Student455 15  13 0.8666667  1.2529245         96       8
#> Student456 Student456 15   4 0.2666667 -2.1272675          3       1
#> Student457 Student457 15  13 0.8666667  1.2529245         96       8
#> Student458 Student458 15  11 0.7333333  0.5017707         74       6
#> Student459 Student459 15  10 0.6666667  0.1261938         57       5
#> Student460 Student460 15   6 0.4000000 -1.3761137         15       3
#> Student461 Student461 15   7 0.4666667 -1.0005368         24       4
#> Student462 Student462 15   8 0.5333333 -0.6249599         32       4
#> Student463 Student463 15   6 0.4000000 -1.3761137         15       3
#> Student464 Student464 15   3 0.2000000 -2.5028444          1       1
#> Student465 Student465 15  10 0.6666667  0.1261938         57       5
#> Student466 Student466 15  13 0.8666667  1.2529245         96       8
#> Student467 Student467 15  12 0.8000000  0.8773476         87       7
#> Student468 Student468 15  13 0.8666667  1.2529245         96       8
#> Student469 Student469 15   5 0.3333333 -1.7516906          8       2
#> Student470 Student470 15   6 0.4000000 -1.3761137         15       3
#> Student471 Student471 15   5 0.3333333 -1.7516906          8       2
#> Student472 Student472 15   6 0.4000000 -1.3761137         15       3
#> Student473 Student473 15  10 0.6666667  0.1261938         57       5
#> Student474 Student474 15   6 0.4000000 -1.3761137         15       3
#> Student475 Student475 15  12 0.8000000  0.8773476         87       7
#> Student476 Student476 15   9 0.6000000 -0.2493831         42       5
#> Student477 Student477 15  11 0.7333333  0.5017707         74       6
#> Student478 Student478 15  14 0.9333333  1.6285014         99       9
#> Student479 Student479 15   7 0.4666667 -1.0005368         24       4
#> Student480 Student480 15  12 0.8000000  0.8773476         87       7
#> Student481 Student481 15   7 0.4666667 -1.0005368         24       4
#> Student482 Student482 15  13 0.8666667  1.2529245         96       8
#> Student483 Student483 15   9 0.6000000 -0.2493831         42       5
#> Student484 Student484 15  11 0.7333333  0.5017707         74       6
#> Student485 Student485 15  14 0.9333333  1.6285014         99       9
#> Student486 Student486 15  13 0.8666667  1.2529245         96       8
#> Student487 Student487 15  11 0.7333333  0.5017707         74       6
#> Student488 Student488 15  11 0.7333333  0.5017707         74       6
#> Student489 Student489 15  11 0.7333333  0.5017707         74       6
#> Student490 Student490 15   9 0.6000000 -0.2493831         42       5
#> Student491 Student491 15   6 0.4000000 -1.3761137         15       3
#> Student492 Student492 15  12 0.8000000  0.8773476         87       7
#> Student493 Student493 15   9 0.6000000 -0.2493831         42       5
#> Student494 Student494 15   9 0.6000000 -0.2493831         42       5
#> Student495 Student495 15   7 0.4666667 -1.0005368         24       4
#> Student496 Student496 15   6 0.4000000 -1.3761137         15       3
#> Student497 Student497 15   7 0.4666667 -1.0005368         24       4
#> Student498 Student498 15   7 0.4666667 -1.0005368         24       4
#> Student499 Student499 15  10 0.6666667  0.1261938         57       5
#> Student500 Student500 15   7 0.4666667 -1.0005368         24       4
```
