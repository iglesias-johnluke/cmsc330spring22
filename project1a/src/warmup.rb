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
    raise Exception, "Not Implemented"
end

def nthmax(n, a)
    raise Exception, "Not Implemented"
end

def freq(s)
    raise Exception, "Not Implemented"
end

def zipHash(arr1, arr2)
    raise Exception, "Not Implemented"
end

def hashToArray(hash)
    raise Exception, "Not Implemented"
end
