class GameBoard
    # @max_row is an `Integer`
    # @max_column is an `Integer`
    attr_reader :max_row, :max_column

    #grid is array of arrays (rows and columns)
    #each sub-array/column items are hashes with keys "isShip" and "attacked"
    #mapping to boolean values
    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @grid = Array.new(max_row){Array.new(max_column){ 
            {"isShip" => false, "attacked" => false} } }
    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
        # puts "ship row: #{ship.start_position.row} ship column: #{ship.start_position.column}"
        @grid[ship.start_position.row - 1][ship.start_position.column - 1]["isShip"] = true
        return true
    end

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
        # check position

        # update your grid

        # return whether the attack was successful or not
        true
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        0
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
        true
    end

    # String representation of GameBoard (optional but recommended)
    def to_s
        output = "    "
        for col in 1..max_column do #print column numbers
            output += "#{col}" + "    "
        end
        output += "\n"
        for row in 0..(max_row - 1) do #print rows/arrays
            output += "#{row + 1}: "
            for col in 0..(max_column - 1) do
                if @grid[row][col]["isShip"] == false
                    output += "-,"
                else
                    output += "B,"
                end
                if @grid[row][col]["attacked"] == false
                    output += "-"
                else
                    output += "A"
                end
                output += "| "
            end
            output += "\n"
        end
        return output
    end
end
