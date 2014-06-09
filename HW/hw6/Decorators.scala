object Decorators {
    
  object profile { 
    
    private var cm: Map[String, Int] =  Map().withDefault(_ => 0)

    def count(name: String) = 
      cm(name)
   
    def reset(name: String) = 
      cm += name -> 0

    def apply[A, B](name: String)(f: A => B) = new Function1[A, B] {
      def apply(x: A) = {
        val n = cm(name)
        cm += name -> (1 + n)
        f(x)
      }
    }
  }
 
  object trace {
    
    private var n: Int = 0
    def apply[A, B](name: String)(f: A => B) : Function1[A, B] = new Function1[A, B] {
    def apply(x: A) : B = {
      for(i <- 0 until n)
        print("| ")
      println(",- " + name + "(" + x + ")")
      n += 1
      try {
        val result = f(x)
	for(i <- 0 until (n-1))
	  print("| ")
	println("`- " + result)
	result
      } catch {
        case e: Exception => throw e
      } finally {
        n -= 1
      }
     } 
    }
    
  }
 
  
  
  object memo {
    
    def apply[A, B](f: A => B) : Function1[A, B] = new Function1[A, B] {
     
     private var cache: Map[A, Either[B, Throwable]] = Map()
     
     def apply (x: A): B = {
       if(cache.contains(x)){
         cache(x) match{
	   case Left(n) => n
	   case Right(e) => throw e
	 }
       }
       else {
         val ret : Either[B, Throwable] = try{
	   Left(f(x))
	 } catch{
	   case e: Throwable => Right(e)
	 }
	 ret match {
	   case Left(n) => {
	     cache += x -> ret
	     n
	   }
	   case Right(n) => {
	     cache += x -> ret
	     throw n
	   }
	 }
       }
     }
    }
   }
  }


