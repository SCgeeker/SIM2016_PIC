1. Download Data Table (CSV) from Qualtricts Survey and MTurk: 
- Orientation_Size_ENG_Critical_December+7%2C+2016_21.29.csv  
Full raw data.  
- meta.csv  
Selected columns in cleaned raw data. 
- Batch_2622082_batch_results.csv  
Data sheet of valid participants.    
- mTurkBatch.R  
Import full raw data and export available raw data.  
- available_full_raw.csv  
Available Raw data to be parsed.  
2. Upload CSV to http://parser.qrtengine.com/; fill in e-mail address and wait. (*Online parser may be unavailable after the end of 2016*)
3. Download parsed file from mailed link.
- available_full_raw_out.csv.gz  
Parsed raw data.  
4. Use "Parse_Json.R" transfer the parsed CSV to importable CSV.  
5. Use readcsv.R import the raw data and export the analytic data.
