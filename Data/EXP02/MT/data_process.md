1. Download Data Table (CSV) from Qualtricts Survey: Data & Analysis.
2. Upload CSV to http://parser.qrtengine.com/; fill in e-mail address and wait.
3. Download parsed file from mailed link. Unzip and get the parsed CSV.
4. Use "Parse_Json.R" separate the valid json from the parsed CSV. Every participant json are seperatedly saved in single txt file.  
5. Upload each txt file to https://konklone.io/json/. Download the single CSV file. (*Online parser may be unavailable after the end of 2016*) 
6. Use readcsv.R import the raw data and export the analytic data.
