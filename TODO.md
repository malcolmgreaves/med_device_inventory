# TODO

* [x] extract and structure Incomplete Sets data
	* convert XLS --> CSV --> JSON (or, at least, xls --> json)
	* do so reliably
	* point is to have a machine-readable format

* [x] load JSON data into queryable format
	* JSON --> database
	* goal is to make it easy to ask question: **"What items are present in these trays for this period of time?"**

* [ ] visualize JSON data, complute data analyses
	* ad-hoc analysis
	* goal is to gather / glean insights
	* specifics TBD thru interactive data explroation

* [ ] calculate costs of missing parts, from JSON data
	* at every point in time (resolution: day), calculate the total cost ($) of all tools that are __not present__
	* goal is to show how much $$$ is lost due to disorganization

