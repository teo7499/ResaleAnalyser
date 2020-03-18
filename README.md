# ResaleAnalyser
This a small scala project that reads CSV from data.gov pertaining to HDB retail prices. Did this for a small challenge presented by my CTO. Barely got to know Scala and this project served to get my feet wet and build some confidence in the language
Probably should've broken the code into more files for readability but I got carried away and it got slightly bloated.

<b>Function of this project:</b>
 - Reading and parsing CSV files
 - Some basic stats on the dataset

You can run this programme if you have SBT to compile and build. I'll post the compiled jar on my website once I get the time.

<b>Interesting Bit(s):</b>
<ul>Line 103 of src/scala/resaleAnalyser/ResaleAnalyser.scala:</ul>
<blockquote>val flats = src.getLines.zipWithIndex.drop(1).flatMap { case (line, idx) => load(idx, line.split(",")) }.toList</blockquote>

CSV lines are processed line by line in load() function, with exception handling done with index generated.

# Future Updates?
This script is hardcoded to read from the provided CSV file from data.gov. If I get the time, I could reuse code from my DataPivot repo to potj pivot the data and handle varying column number/sizes.
