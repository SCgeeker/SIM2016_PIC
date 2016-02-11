Simulation 2016 EXP 1 Design
===

### Variables for data analysis

| Variable | Values | Description |
|:---:|:---:|:---:|
| Perspectives | 1,3 | Subject is 1st or 3rd person. **BS**|
| Actions | Obs, Mov | Type of verb. Obs = observation, Mov = Move. **BS**|
| Size | L, S | Size of object. L = Large, S = Small. **WS** |
| Pic | Default,Roated | Orientations of target picture. d = default, r = rotated. **WS** |
| Match | Y, N | Matching of probe and picture. Y = Match, N = Mismatch. **WS** |  

**BS**: Between-subject variable; **WS**: Within-subject variable

### Variables for classification

| Variable | Values | Description |
|:---:|:---:|:---:|
| Languages | Chinese, English | Version of experiment |
| ID | string | Identify the participant who contribute data |
| List | 1,2,3,4 | Arranged by counterbalanced priciple. Stimulus assgined to subject in a within-subject condition. **Sequence object** | 
| Probe | Printed sentences | Arranged in each *List* |
| Target | Filenames of pictures | Path in each *List*. Files are in the external diectory. |
| correct_response| /,z | Response keys for verification task. / = 'yes', z = 'no'. |
