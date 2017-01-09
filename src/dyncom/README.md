dynamic-community
=================

_Dynamic Community Tracking Tool_



The Dynamic Community Tracking Tool is a console application written in C++ for identifying and tracking communities of nodes in dynamic networks, where these networks are represented as a set of step graphs representing snapshots of the network at successive time periods.





### Compilation 

To compile all of the command line tools, in the source root directory run:

	make all

### Usage

The main **tracker** tool is run from the command line as follows:

	./tracker -t [matching_threshold] -o [output_prefix] step1_communities step2_communities ...

**Parameter explanation:**

- The optional parameter *matching_threshold* is a value [0,1] indicating the threshold required to match communities between time steps. A higher value indicates a more conservative matching threshold. Low values are suitable for data where community memberships are expected to be transient over time, high values are suitable where community memberships are expected to be consistent over time. The default threshold value is 0.1.






	5 3 6 7 8 9
	10 11 12 1 4

	M2:2=2,3=1





- The mandatory parameter *timeline_file* corresponds to the name of the output file from the tracker tool. The default prefix is "dynamic".


- The optional parameter *persist_threshold* specifies the proportion of time steps required for a node to be deeemed to be a member of a community. By default, a node is only required to appear in a single time step community. 

- The optional parameter *min_length* is an integer indicating the minimum length (in terms of number of steps) for a dynamic community to be included in the final results. By default, a dynamic community must be present in at least two time steps.




The resulting output file, res.persist, contains communities in the same format as the original input file (i.e. one community per line, specified in terms of node identifiers):

	1 2 3 4
	5 6 7 8 9
	10 11 12

