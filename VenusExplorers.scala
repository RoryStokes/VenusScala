import explorer.Explorer

object VenusExplorers extends App {

	//Defining regular expressions to read in each of input line types
	val gridInitPattern     = "([0-9]+) ([0-9]+)$".r
	val explorerInitPattern = "([0-9]+) ([0-9]+) ([NSEW])$".r
	//If a command line argument is specified, loads the source from that path, otherwise uses stdin as source
	val source = if(args.length == 0) io.Source.stdin.getLines() else io.Source.fromFile(args(0)).getLines()
	

	// FIRST LINE OF INPUT 

	//Uses a regular expression to match the first line of input
	val corner = source.next match {
		//if a match is found, store the x and y coordinates
		case gridInitPattern(x,y) =>
			(x,y)
		//otherwise, output an error and end the program's execution
		case _ =>
			println("Invalid input in line 1. Line should contain: [right coord] [top coord]")
			System.exit(0)
	}

	// SUBSEQUENT LINES OF INPUT

	//Use the recursive getExplorers function to generate a list of Explorer objects
	val explorers = getExplorers(source)
	//Output the list of explorers to stdout
	println(explorers mkString "\n")


	// FUNCTION DEFINITIONS


	/** Produces a list of Explorers based on the input strings provided.
	  *
	  * Operates recursively, loading in two lines for each explorer (definition and moves)
	  *
	  * @param source the input strings to derive from
	  * @return a list of Explorer objects
	  */
	def getExplorers(source: Iterator[String]) : List[Explorer] = {
		if(source.hasNext){
			//As long as there are more lines to read, read the initial properties for a new explorer
			val explorerInitPattern(x,y,direction) = source.next
			//Create a new explorer based on the initial conditions, load in the next line as the movement
			//instructions. Prepend to list and recurse.
			new Explorer(x.toInt, y.toInt, direction(0)).move(source.next) :: getExplorers(source)
		} else {
			//Base case: at end of iterator simply return an empty list
			List[Explorer]()
		}
	}
}
