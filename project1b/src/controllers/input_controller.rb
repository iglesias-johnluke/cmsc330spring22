require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    # GameBoard.new 10, 10
    gameBoard = GameBoard.new(10, 10)
    ships_added = 0
    read_file_lines(path){ |line|
        if line =~ /^\(([1-9]),([1-9])\), (Right|Left|Up|Down), ([1-9])$/ and ships_added < 5
            # puts "(" + $1 + "," + $2 + "), " + $3 + ", " + $4
            newShip = Ship.new( Position.new($1.to_i, $2.to_i), $3, $4.to_i)
            if gameBoard.add_ship(newShip) == true
                ships_added += 1
            end
        end   
    }
    if ships_added < 5
        return nil
    end
    return gameBoard
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    output = []
    if read_file_lines(path) == false
        return nil
    end
    read_file_lines(path){|line|
        if line =~ /^\(([1-9]),([1-9])\)$/
            newPosition = Position.new($1.to_i, $2.to_i)
            output.append(newPosition)
        end
    }
    return output
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
