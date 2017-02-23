# Medical Device Inventory Analysis (MEDIA)

Statistical analysis of medical device and surgical equipment inventory over time.



### Sections

There are several sections within this project:

* See [TODO](TODO.md) for current work status.

* See [Objectives](OBJECTIVES.md) for high-level objectives and diuscussion thereof.

### Data Preprocessing

To convert the "incomplete sets" .xls files to .csv files, use LibreOffice calc. The specific command is:
```
libreoffice  --convert-to csv --infilter=CSV:44,34,76,1 --headless --outdir $X *.xls
```
where `$X` is the desired output directory
