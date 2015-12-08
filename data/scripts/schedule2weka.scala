val lines = io.Source.fromFile(args(0)).getLines

val carriers = args.tail.toSet
  
val read_ = lines.toList.map{
  l => l.split(',') match{
    case Array(a, c, m) =>
      val (car, sem) = c.span(_.isLetter)
      (a, (car, sem.trim, "'" + m + "'"))
    case Array(a, c, ms@ _*) =>
      val (car, sem) = c.span(_.isLetter)
      (a, (car, sem.trim, ms.mkString(",")))
  }
}

val showCarrier = carriers.size != 1

val read = read_.filter(carriers contains _._2._1)

val semesters   = read.map(_._2._2).distinct
val disciplines = read.map(_._2._3).distinct


val g = read.toSeq.groupBy(_._1)

val toWrite = g.map{
  case (_, vs) => 
    val (_, pps) = vs.unzip
    val (cs, ss, ms) = pps.unzip3
    val c = cs.distinct.ensuring(_.length == 1).head
    val s = ss.distinct.ensuring(_.length == 1).head
    val ds = disciplines.map{
      case d if ms contains d => "t"
      case _                  => "?"
    }
    val left = if(showCarrier) Seq(c, s) else Seq(s)
    (left ++ ds).mkString(",")
}



println("@relation 'course-selection-itesm'")

println("% " + disciplines.size + " disciplines")
println("% " + g.size + " students")

if(showCarrier) println("@attribute carrier { " + carriers.mkString(",") + " }")
println("@attribute semester { " + semesters.mkString(",") + " }")
disciplines foreach { d => println("@attribute " + d + " { t }")}

println("\n@data")
toWrite foreach println