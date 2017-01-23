### Objective

**Objective is: we can see which tools are in which trays over time.**

### Implementation

- Pull out all records related to WENTXXX trays (sets)
- Compare these trays week-to-week
    - See which tools are MISSING from each tray (use part #s)
    - Track by week
- Associate cost to per-tool

### Data Format
     * SHARED COLUMNS *

- Column A (through G?): Serial # for the type of tray (e.g. WENT069)
- Column R: specific tray name (identifier...?)
    - Other mode: instrument name
- Column L: serial number for type of instrument (NOT EXACT unique instrument)
- Column B: position #
- Column D: target # (# of the tool that should be in the tray)
- Column F-G: account # (actual # of instances of the tool in the tray)
- Column I-J: "No." (how short we are: difference between target and account)

### Deliverables

- Structured dataset
- Programs:
    - Program 1: xls to JSON
    - Program 2: JSON files at dates to queryable format
    - Program 3: JSON files at dates to visualizations and pre-set analyses
    - Program 3: JSON files at dates + part costs file to amount of money we're losing
- Analyses
    - Visualizations
        - # instruments missing over time
            - partitioned by tray type (e.g. WENTXXX)
        - Interactive: slice by time, show by tray


