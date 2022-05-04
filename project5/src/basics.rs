/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 0 {
        return -1
    }else if n == 1{
        return 1
    }
    return n + gauss(n - 1)


}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut total = 0;
    for item in ls.iter(){
        if item >= &s && item <= &e{
            total = total + 1
        }
    }
    return total;
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    for item in target.iter(){
        if set.contains(item) == false{
            return false
        }
    }
    return true
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    if ls == []{
        return None
    }
    let sum : f64 = ls.iter().sum();
    let length = ls.len() as f64;
    return Some(sum/length) 
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    if ls == []{
        return 0
    }
    let mut str = "".to_owned();

    for item in ls.iter(){
        str.push_str(&item.to_string());
    }
    let intval = isize::from_str_radix(&str, 2).unwrap();
    return intval as i32
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut output : Vec<u32> = Vec::new();
    let mut i = 2;
    let mut n = n;
    while n > 1 {
        if n % i == 0{
            output.push(i);
            n = n/i;
            i = i - 1;
        }
        i = i + 1;
    }
    return output
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut output : Vec<i32> = Vec::new();
    if lst == []{
        return output;
    }
    let first = lst[0];
    for i in 1..(lst.len()){
        output.push(lst[i]);
    }
    output.push(first);
    return output
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    if target == ""{
        return true
    }if target.len() > s.len(){
        return false
    }
    let mut output = false;
    for (i, item) in s.char_indices() {
        if item.to_string() == &target[0..1] && (i + target.len()) <= s.len(){
            let curr_string = &s[ i..(i + target.len()) ];
            if curr_string == target{
                output = true;
            }
        }
    }
    return output;
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    if s == ""{
        return None
    }
    let mut longestStr = String::new();
    longestStr.push(s.chars().nth(0).unwrap());
    
    let mut longestStart = 0;
    let mut tmpStart = 0;
    
    let mut tmpStr = String::new();
    tmpStr.push(s.chars().nth(0).unwrap());
    
    for i in 1..s.len(){
        let currChar = &s[i..i+1];
        let prevChar = &s[i-1..i];
        if currChar == prevChar{
            tmpStr.push(s.chars().nth(i).unwrap());
        }else{
            if tmpStr.len() > longestStr.len(){
                longestStr = tmpStr.clone();
                longestStart = tmpStart;
            }
            tmpStr = s[i..i+1].to_string();
            tmpStart = i;
        }
    }
    if tmpStr.len() > longestStr.len(){
        longestStr = tmpStr.clone();
        longestStart = tmpStart;
    }
    
    return Some(&s[longestStart..(longestStart + longestStr.len())]);

}
