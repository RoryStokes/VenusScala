package explorer

class Explorer(var x : Int, var y : Int, var dir : Char) {

	def move(commands: String) : Explorer = {
		if(commands.length >= 1){
			commands(0) match{
				case 'L' => turnLeft
				case 'R' => turnRight
				case 'M' => forward
				case _ => throw new IllegalArgumentException("command not recognised")
			}
			move(commands.substring(1))
		}
		this
	}

	def turnLeft() = {
		dir = dir match{
			case 'N' => 'W'
			case 'E' => 'N'
			case 'S' => 'E'
			case 'W' => 'S'
		}
	}

	def turnRight() = {
		dir = dir match{
			case 'N' => 'E'
			case 'E' => 'S'
			case 'S' => 'W'
			case 'W' => 'N'
		}
	}

	def forward() = {
		dir match{
			case 'N' => y += 1
			case 'E' => x += 1
			case 'S' => y -= 1
			case 'W' => x -= 1
		}
	}
	override def toString = s"$x $y $dir"
}
