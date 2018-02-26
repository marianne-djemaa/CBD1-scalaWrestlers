class Country(val name: String, var wrestlers: List[Wrestler] = List()){
  var cities = List()

  override def toString: String = {
    var countryString = name
    if (wrestlers.size > 0) {
      countryString = countryString + " wrestlers:\n"
      for (i <-0 until wrestlers.size) {
        countryString = countryString + "- " + wrestlers(i) + "\n"
      }
    }
    return countryString
  }

  //  def addCity(c:City) //TODO
  //
  //  def addWrestler(w:Wrestler) //TODO

}