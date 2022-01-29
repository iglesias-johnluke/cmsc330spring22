class PhoneBook
    def initialize
        @book = {}
    end

    def add(name, number, is_listed)
        if @book.has_key?(name) == false and isValidPhoneNumber(number) and hasListingConflict(number, is_listed) == false
            @book[name] = [number, is_listed]
            return true
        end
        return false

    end

    #returns true if number is in valid format, false otherwise
    def isValidPhoneNumber(number)
        numsStr = "1234567890"
        if number.length != 12
            return false
        elsif number[3] != '-' || number[7] != '-'
            return false
        else
            (number[0..2]).split('').each{ |char|
                if numsStr.include?(char) == false
                    return false
                end
            }
            (number[4..6]).split('').each{ |char|
                if numsStr.include?(char) == false
                    return false
                end
            }   
            (number[8..11]).split('').each{ |char|
                if numsStr.include?(char) == false
                    return false
                end
            }
        
        end
        return true
    end

    #returns true if number cannot be listed, false otherwise
    def hasListingConflict(number, is_listed)
        if is_listed
            bookValues = @book.values
            #loop thru all values in book, check if book already has number
            #and that number is_listed
            for array in bookValues 
                if array[0] == number and array[1] == true #return hasConflict
                    return true
                end
            end
        end
        return false
    end
    

    def lookup(name)
        raise Exception, "Not implemented"
    end

    def lookupByNum(number)
        raise Exception, "Not implemented"
    end

    def namesByAc(areacode)
        raise Exception, "Not implemented"
    end
end
