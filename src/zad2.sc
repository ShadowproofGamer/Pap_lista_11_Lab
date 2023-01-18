case class Vector(var v:Array[Int]):

  var content = v;

  def +(other: Vector): Vector = {

    if (other.content.length > content.length) then Vector(adding_rec(0, other.content, content, new Array[Int](0)))
    else Vector(adding_rec(0, content, other.content, new Array[Int](0)))
  }

  def -(other: Vector): Vector = {
    if (other.content.length > content.length) then Vector(adding_rec(0, other.content.map(x => -x), content, new Array[Int](0)))
    else Vector(adding_rec(0, content, other.content.map(x => -x), new Array[Int](0)))
  }

  def adding_rec(i: Int, vLonger: Array[Int], vShorter: Array[Int], result: Array[Int]): Array[Int] = {
    if (i < vShorter.length) adding_rec(i + 1, vLonger, vShorter, result :+ (vLonger(i) + vShorter(i)))
    else if (i < vLonger.length) adding_rec(i + 1, vLonger, vShorter, result :+ (vLonger(i) + 0))
    else result
  }
  /*
  def *+(other: Vector):Int = {
    other.content
  }
  */