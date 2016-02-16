/** Provides the Explorer class to store positional information and handle movement instructions
  *
  * ==Overview==
  * The class is [[explorer.Explorer]]
  * Manipulation should be via the move method.
  */
package explorer


/** An individual explorer.
  *
  * @constructor create a new explorer with the given positional information
  * @param x the x coordinate
  * @param y the y coordinate
  * @param dir a Char representing the current facing direction as a cardinal compass direction (N/S/E/W)
  */
class Explorer(var x : Int, var y : Int, var dir : Char) {

	//Ensure that the facing direction character is valid
	if( !("NSEW" contains dir)){
		throw new IllegalArgumentException("Invalid input in explorer initialisation. Initial facing direction invalid.")
	}

	/** Move the explorer according to a string of commands
	  *
	  * Each character (L/R/M) corresponds to a single motion of the explorer
	  *
	  * @param commands a string of characters (L/R/M) specifying the instructions for the explorer's movements
	  * @return the Explorer object
	  */
	def move(commands: String) : Explorer = {
		//as long as there are commands to execute
		if(commands.length >= 1){
			//execute the first command in the string
			commands(0) match{
				//depending on the character, call the relevant movement method
				case 'L' => turnLeft
				case 'R' => turnRight
				case 'M' => forward
				case _ => throw new IllegalArgumentException("Invalid input in explorer movement instructions. Line should only contain characters 'L', 'R' and 'M'")
			}
			//recurse with the remaining commands
			move(commands.substring(1))
		}
		this
	}

	/** Turns the explorer 90 degrees to the left
	  * @return the new direction
	  */
	def turnLeft() = {
		dir = dir match{
			case 'N' => 'W'
			case 'E' => 'N'
			case 'S' => 'E'
			case 'W' => 'S'
		}
	}

	/** Turns the explorer 90 degrees to the right
	  * @return the new direction
	  */
	def turnRight() = {
		dir = dir match{
			case 'N' => 'E'
			case 'E' => 'S'
			case 'S' => 'W'
			case 'W' => 'N'
		}
	}


	/** Moves the explorer one unit in the direction it is facing.
	  * @return Unit
	  */
	def forward():Unit = {
		dir match{
			case 'N' => y += 1
			case 'E' => x += 1
			case 'S' => y -= 1
			case 'W' => x -= 1
		}
	}

	//Allows for implicit conversion to an output string
	override def toString = s"$x $y $dir"
}
