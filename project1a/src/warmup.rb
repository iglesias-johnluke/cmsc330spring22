def fib(n)
    num1 = 0
    num2 = 1
    count = 0
    arr = Array.new(0)
    if n == 1
        arr.push(0)
    elsif n == 2
        arr.push(0, 1)
    elsif n > 2
        arr.push(0, 1)
        count = 2
        while count < n
            newNum = num1 + num2
            arr.push(newNum)
            num1 = num2
            num2 = newNum
            count += 1
        end   
    end
    return arr
end

def isPalindrome(n)
    return n.to_s == n.to_s.reverse
end

def nthmax(n, a)
    if n > a.length() - 1
        return nil
    else
        a.sort!
        a.reverse!
        return a[n]
    end
end

def freq(s)
    output = ""
    if s == ""
        return output
    else
        charArray = s.split('')
        hashMap = Hash.new
        charArray.each { |char| #loop thru chars, map char to num occurences in map
            if hashMap.key?(char) 
                hashMap[char] += 1
            else
                hashMap[char] = 1
            end
        }
        maxCount = hashMap.values.max #get largest count in map, find corresponding key
        hashMap.keys.each { |char|
            if hashMap[char] == maxCount
                return char
            end
        }
    end
end

def zipHash(arr1, arr2)
    hashMap = {}
    if arr1.length == 0 and arr2.length == 0 
        return hashMap
    elsif arr1.length != arr2.length
        return nil
    else
        index = 0
        for item in arr1 do
            hashMap[item] = arr2[index]
            index += 1 
        end
        return hashMap
    end
end

def hashToArray(hash)
    output = []
    if hash.keys.length != 0
        hash.keys.each{ |key|
            output.push( [key, hash[key]] )

        }
    end
    return output
end
