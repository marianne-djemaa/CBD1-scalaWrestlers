class Wrestler(var name: String, var country: Country = new Country("UNKNOWN"), var birthdate: String = "UNKNOWN", var gender: String = "UNKNOWN",
               var height: String = "UNKNOWN", var weight: String = "UNKNOWN", var debut: String = "UNKNOWN", var retired: String = "UNKNOWN", var billedFrom: String = "UNKNOWN") {


  override def toString: String = {
    var s = new StringBuilder(name)
    if (gender != "UNKNOWN") s ++= ", " + gender
    if (country.name != "UNKNOWN") s ++= ", " + country.name
    if (birthdate != "UNKNOWN") s ++= ", born " + birthdate
    if (height != "UNKNOWN") s ++= ", height: " + height
    if (weight != "UNKNOWN") s ++= ", weight: " + weight
    if (billedFrom != "UNKNOWN") s ++= ", ville: " + billedFrom
    if (debut != "UNKNOWN") s ++= ", debut: " + debut
    if (retired != "UNKNOWN") s ++= ", retired: " + retired
    return s.toString()
  }

  def to_csv: String = {
    //todo continuer ici (trouver moyen de concatener plusieurs strings avec un separator
    return ""
  }

  //  might be used to store Wrestler instances in a .json file
  //  next step: constructor from .json file for the class
  def to_json: String = {
    var s = new StringBuilder("{")
    s ++= "\"COUNTRY\": \"" + country.name +"\""
    s ++= "\"WRESTLER\": \"" + name +"\""
    s ++= "\"BIRTHDATE\": \"" + birthdate +"\""
    s ++= "\"HEIGHT\": \"" + height +"\""
    s ++= "\"WEIGHT\": \"" + weight +"\""
    s ++= "\"BILLEDFROM\": \"" + billedFrom +"\""
    s ++= "\"DEBUT\": \"" + debut +"\""
    s ++= "\"RETIRED\": \"" + retired +"\""
    return s.toString()
  }

}