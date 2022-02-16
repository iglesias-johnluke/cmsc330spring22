class GameBoard
    # @max_row is an `Integer`
    # @max_column is an `Integer`
    attr_reader :max_row, :max_column
    attr_accessor :grid, :num_successful_attacks, :num_possible_attack_hits

    #grid is array of arrays (rows and columns)
    #each sub-array/column items are hashes with keys "isShip" and "attacked"
    #mapping to boolean values
    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @grid = Array.new(max_row){Array.new(max_column){ 
            {"isShip" => false, "attacked" => false} } }
        @num_successful_attacks = 0
        @num_possible_attack_hits = 0
    end

    #returns true if ship wont overlap other ships and theres space
    #on board for ship, false otherwise
    def canAddShip(ship)
        rowIndexStart = ship.start_position.row - 1
        colIndexStart = ship.start_position.column - 1
        shipLength = ship.size - 1

        #check no overlaps and within bounds
        if ship.orientation == "Right"
            for col in (colIndexStart..(colIndexStart + shipLength)) do
                if (colIndexStart + shipLength) >= (@max_column) || @grid[rowIndexStart][col]["isShip"] == true
                    return false
                end
            end
        elsif ship.orientation == "Left"
            for col in ((colIndexStart - shipLength)..colIndexStart) do
                if (colIndexStart - shipLength) < 0 || @grid[rowIndexStart][col]["isShip"] == true
                    return false
                end
            end
        elsif ship.orientation == "Up"
            for row in ((rowIndexStart - shipLength)..rowIndexStart) do
                if (rowIndexStart - shipLength) < 0 || @grid[row][colIndexStart]["isShip"] == true
                    return false
                end
            end
        elsif ship.orientation == "Down"
            for row in (rowIndexStart..(rowIndexStart + shipLength)) do
                if (rowIndexStart + shipLength) >= (@max_row) || @grid[row][colIndexStart]["isShip"] == true
                    return false
                end
            end
        end

        return true
    end
    

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
        rowIndexStart = ship.start_position.row - 1
        colIndexStart = ship.start_position.column - 1
        shipLength = ship.size - 1

        if canAddShip(ship) == false #dont add ship if board doesnt allow
            return false
        end

        #add ship coordinates
        if ship.orientation == "Right"
            for col in (colIndexStart..(colIndexStart + shipLength)) do
                @grid[rowIndexStart][col]["isShip"] = true
            end
        elsif ship.orientation == "Left"
            for col in ((colIndexStart - shipLength)..colIndexStart) do
                @grid[rowIndexStart][col]["isShip"] = true
            end
        elsif ship.orientation == "Up"
            for row in ((rowIndexStart - shipLength)..rowIndexStart) do
                @grid[row][colIndexStart]["isShip"] = true
            end
        elsif ship.orientation == "Down"
            for row in (rowIndexStart..(rowIndexStart + shipLength)) do
                @grid[row][colIndexStart]["isShip"] = true
            end
        end
        @num_possible_attack_hits += ship.size
        return true
    end

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
        # check position
        if position.row > @max_row || position.column > @max_column
            return nil
        end
        # update your grid
        if @grid[position.row - 1][position.column - 1]["isShip"] == true and
            @grid[position.row - 1][position.column - 1]["attacked"] == false
            
            @num_successful_attacks += 1
            @grid[position.row - 1][position.column - 1]["attacked"] = true
            return true
        else
            @grid[position.row - 1][position.column - 1]["attacked"] = true
            return false
        end
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        return @num_successful_attacks
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
        return @num_successful_attacks == @num_possible_attack_hits
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
