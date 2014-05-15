def last ( list: List[Int] ): Int = {
  list match {
    case Nil => throw new NoSuchElementException
    case head :: Nil => head
    case head :: tail => last( tail )
  }
}
assert ( last ( List ( 1,2,3,4,5 ) ) == 5 )
assert ( last ( List ( 1,2,3,4,5,6 ) ) == 6 )
assert ( last ( List ( 1,2,3,4,5,6,6,5 ) ) == 5 )
assert ( last ( List ( 1,2,3,4,5,7,7 ) ) == 7 )

def penultimate( list : List[Int] ) : Int = {
  list match {
    case Nil => throw new NoSuchElementException
    case head :: Nil => throw new NoSuchElementException
    case head :: tail :: Nil => head
    case head :: tail =>  penultimate(tail)
  }
}

assert( penultimate( List( 1,2,3,4,5 ) ) == 4)
assert( penultimate( List( 1,2,3,4,5,6 ) ) == 5)
assert( penultimate( List( 1,2,3,4,5,6,6,5 ) ) == 6)
assert( penultimate( List( 1,2,3,4,5,7,7 ) ) == 7)

def reverse ( list : List[Any] ) : List[Any] = {
  list match {
    case Nil => Nil
    case head::Nil => List(head)
    case head :: tail => reverse( tail ) ::: List( head )
  }
}

assert( reverse( List( 1,2,3,4,5 ) ) ==   List(5,4,3,2,1) )
assert( reverse( List( 1,2,3,4,5,6 ) ) == List(6,5,4,3,2,1) )
assert( reverse( List( 1,2,3,4,5,6,6,5 ) ) == List(5,6,6,5,4,3,2,1) )
assert( reverse( List( 1,2,3,4,5,7,7 ) ) == List(7,7,5,4,3,2,1) )

def compress ( list : List[Any] ) : List[Any] = {
  list match {
    case Nil => Nil
    case head :: Nil => List(head)
    case first::second::tail =>
      if( first == second )
        compress ( List( second ) ::: tail )
      else
        List( first ) ::: compress ( List ( second ) ::: tail )
  }
}

assert( compress(List(1,1,1,1)) == List(1))
assert( compress(List(1,2,2,3,3,1,1,1)) == List(1,2,3,1))
assert( compress(List(1,1,2,2,1,1,2,2)) == List(1,2,1,2))
assert( compress(List(3,3,3,3,3,3,3,3,31,1,1)) == List(3,31,1))

def duplicate_single_element ( n:Int, element:Any ) : List[Any] =  {
  n match {
    case 0 => Nil
    case n => List(element) ::: duplicate_single_element(n-1 , element)
  }
}
def duplicate ( n:Int , list : List[Any] ) : List[Any] = {
  (n,list) match {
    case ( _,Nil ) => Nil
    case( n, head::tail ) => duplicate_single_element( n,head ):::duplicate( n,tail )
  }
}

assert( duplicate (3,List(1,1)) == List(1,1,1,1,1,1))
assert( duplicate (2,List(1,2,3)) == List(1,1,2,2,3,3))
assert( duplicate (4,List(2,1,2)) == List(2,2,2,2,1,1,1,1,2,2,2,2))
assert( duplicate (5,List(2)) == List(2,2,2,2,2))


def delete( n:Int ,indexfromstart:Int, list:List[Any]) : List[Any] = {

  (n, indexfromstart, list ) match {

    case (0, _, _) => throw new NoSuchElementException
    case (_, _, Nil ) => Nil
    case (n, 1, head :: tail )  => delete( n, n, tail)
    case (n, indexfromstart, head :: tail ) => List( head ) ::: delete( n, indexfromstart-1, tail)
  }
}
def drop(n :Int, list : List[Any] ) : List[Any] = {
  delete(n, n, list )
}

assert(drop(18,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
assert(drop(10,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14))
assert(drop(5,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(1, 2, 3, 4, 6, 7, 8, 9, 11, 12, 13, 14))
assert(drop(6,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 13, 14))


def slice( start : Int, end : Int, list: List[Any]): List[Any] = {

  (start, end, list) match {
    case (_, _, Nil) => Nil
    case (_, 0, _) => Nil
    case (0, _, head :: tail) => List(head) ::: slice(0, end-1, tail)
    case (_, _, head :: tail) => slice(start-1, end-1, tail)
  }
}

assert(slice(0,15,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
assert(slice(1,10,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(2, 3, 4, 5, 6, 7, 8, 9, 10))
assert(slice(3,5,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(4, 5))
assert(slice(3,4,List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == List(4))

// isprime checks if p is a prime

def isPrime(p: Int, k: Int): Boolean ={
  (p, k) match {
    case (_, 0) => false
    case (_, 1) => true
    case (_,_) if(p%k == 0) => false
    case (_,_) if(p%k != 0) => isPrime(p,k-1)
  }
 }

def listPrimesinRange(start: Int, end: Int ) : List[Any] = {
    if(start > end)
      return Nil

    else  if( isPrime(start ,start/2 ) )
      return List(start) ::: listPrimesinRange(start+1, end)

    else
      return  listPrimesinRange(start+1, end)
  }

assert(listPrimesinRange(4,13) == List(5,7,11,13))
assert(listPrimesinRange(7,31) == List(7,11,13,17,19,23,29,31))
assert(listPrimesinRange(4,31) == List(5,7,11,13,17,19,23,29,31))
assert(listPrimesinRange(1,10) == List(2,3,5,7))

def multiplesofthreeorfive( n:Int ) : Int = {

  if(n == 0)
    return 0

  if( n%3 == 0 || n%5 == 0)
    return n + multiplesofthreeorfive(n-1)

  return multiplesofthreeorfive(n-1)
}

assert(multiplesofthreeorfive ( 999 ) == 233168 )
assert(multiplesofthreeorfive ( 10 ) == 33 )
assert(multiplesofthreeorfive ( 20 ) == 98 )
assert(multiplesofthreeorfive ( 30 ) == 225 )

def numberofpaths ( length: Int, breadth: Int): Int = {

  if ( length == 1 || breadth ==1)
    return breadth + breadth

  else
    return ( numberofpaths ( length -1, breadth ) + numberofpaths (length , breadth-1 ) )
}

assert( numberofpaths ( 10, 10 ) == 184756)
assert( numberofpaths ( 5, 5 ) == 252)
assert( numberofpaths ( 2, 2 ) == 6)
assert( numberofpaths ( 3, 3 ) == 20)
def fullWords(number: Int): List[String] = {
    if(number<10) {
      if (number == 0)
        return List("zero")
      else if (number == 1)
        return List("one")
      else if (number == 2)
        return List("two")
      else if (number == 3)
        return List("three")
      else if (number == 4)
        return List("four")
      else if (number == 5)
        return List("five")
      else if (number == 6)
        return List("six")
      else if (number == 7)
        return List("seven")
      else if (number == 8)
        return List("eight")
      else  return List("nine")
    }

    else{
      if(number %10 == 0)
       return fullWords(number/10) ::: List("zero")
     else if ( number %10 == 1)
      return fullWords(number/10) ::: List("one")
     else if( number %10 == 2)
       return fullWords(number/10) ::: List("two")
     else if( number %10 == 3)
       return fullWords(number/10) ::: List("three")
      else if( number %10 == 4 )
        return fullWords(number/10) ::: List("four")
     else if(number %10 == 5)
       return fullWords(number/10) ::: List("five")
     else if(number %10 == 6)
       return fullWords(number/10) ::: List("six")
     else if(number %10 == 7)
        return fullWords(number/10) ::: List("seven")
     else if(number %10 == 8)
      return fullWords(number/10) ::: List("eight")
     else return fullWords(number/10) ::: List("nine")

  }
}

assert( fullWords ( 100 ) == List( "one","zero","zero" ) )
assert( fullWords ( 132 ) == List( "one","three","two" ) )
assert( fullWords ( 1432 ) == List( "one","four","three","two" ) )
assert( fullWords ( 4312 ) == List( "four","three","one","two" ) )

