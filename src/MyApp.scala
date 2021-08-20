object MyApp{
  def main(args:Array[String]): Unit ={
    val r1 = new Rational(1,2)
    val x = new Rational(3,4)
    val y = new Rational(5,8)
    val z = new Rational(2,7)
    val ac1 = new Account("ID1",1,100)
    val ac2 = new Account("ID2",2,200)
    val ac3 = new Account("ID3",3,2000)
    val ac4 = new Account("ID4",4,-1200)
    val ac5 = new Account("ID5",5,5300)
    val ac6 = new Account("ID6",6,-100)
    val ac7 = new Account("ID7",7,2200)
    val ac8 = new Account("ID8",8,4500)
    val ac9 = new Account("ID9",9,3750)

    var bank:List[Account] = List(ac1,ac2,ac3,ac4,ac5,ac6,ac7,ac8,ac9)


    println("Answer for Question Number 1\n-----------------------------")
    println("Negation of (1,2) is : "+r1.neg)

    println("\nAnswer for Question Number 2\n-----------------------------")
    println("(3,4)-(5,8)-(2,7) : "+(x-y-z))

    println("\nAnswer for Question Number 3\n-----------------------------")
    println("Initial balance of ac1 : "+ac1.balance)
    println("Initial balance of ac2 : "+ac2.balance)
    println("Transaction-> 50LKR from ac2 to ac1 happenning.....")
    ac2.transfer(ac1,50)
    println("The balance of ac1 : "+ac1.balance)
    println("The balance of ac2 : "+ac2.balance)
    println("Transaction Successfully done!\n")

    println("\nAnswer for Question Number 4\n-----------------------------")
    println("Question Number 4.1\nOverdraft List : \n")
    overdraft(bank).foreach(i => println(i))
    println("\nQuestion Number 4.2\nSum of balances in all accounts : "+totalBalance(bank)+"\n")
    println("Question Number 4.3\nFinal balances of the account after applying the interest : "+totalBalance(interest(bank)))
  }

  val overdraft=(b:List[Account])=>b.filter(x=>x.balance<0)
  val totalBalance=(b:List[Account])=>b.reduce((x,y)=>new Account("",0,x.balance+y.balance)).balance
  val interest=(b:List[Account])=>b.map(x=>if(x.balance>=0) new Account(x.nic,x.acnumber,x.balance+x.balance*0.005) else new Account(x.nic,x.acnumber,x.balance+x.acnumber*0.001) )

}

class Rational(n:Int,d:Int){
  def numer = n
  def denom = d

  def neg = new Rational(-this.numer,this.denom)

  def -(r:Rational) = new Rational(this.numer*r.denom-r.numer*this.denom,this.denom*r.denom)
  override def toString = numer+"/"+denom
}

class Account(id:String,n:Int,b:Double){
  val nic : String = id
  val acnumber : Int = n
  var balance: Double = b

  def transfer(a:Account,b:Double) = {
    this.balance = this.balance - b
    a.balance = a.balance + b
  }

  override def toString = "["+nic+":"+acnumber+":"+balance+"]"
}





