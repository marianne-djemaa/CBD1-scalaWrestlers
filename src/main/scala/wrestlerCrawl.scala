object wrestlerCrawl{
  import scala.io.Source
  import util.control.Breaks._

  /*
  Retourne une String correspondant au contenu d'une page web dont l'url est passee en parametre
   */
  def urlToString(url:String): String ={
    val urlText = Source.fromURL(url)
    return urlText.mkString
  }

  /*
  Retourne une liste contenant les titres des pages appartenant a une categorie Wikipedia passee en parametre
  (utilise l'API wiki pour recuperer la liste de ces pages au format xml, qui est ensuite splittee)
   */
  def getPagesFromCategory(category:String): List[String] ={
    val xmlWikiCat = urlToString("https://en.wikipedia.org/w/api.php?action=query&format=xml&list=categorymembers&cmtitle=" + makeWikiLike(category) + "&cmlimit=max")
    val xmlLines = xmlWikiCat.split(">")
    var pages:List[String] = List()
    for (i <-0 until xmlLines.length) {
      val myLine = xmlLines(i).trim()
      if (myLine.startsWith("<cm pageid=")){
        var splitLine = myLine.split("\"")
        pages = pages :+ splitLine(5) //champ correspondant au titre de la page
      }
    }
    return pages
  }

  /*
Renvoie une String correspondant à la nationalité dans une chaîne de type "Category:New Zealand female professional wrestlers"/"Category:New Zealand male professional wrestlers"
 */
  def getNationalityFromCategory(category:String, splitWord:String): String ={
    return category.substring(9).split(" "+splitWord)(0)
  }

  /*
  Renvoie une String dans laquelle les caractères ":" et " " ont été remplacés par leur équivalent dans une URL wiki
   */
  def makeWikiLike(originalString:String): String ={
    return originalString.replace(":", "%3A").replace(" ","%20")
  }

  /*
  Retourne un objet de type Wrestler créé à partir de la page wikipedia correspondant à son nom.
  A terme, permettra de remplir, en plus de la date de naissance, les champs taille et poids d'un objet Wrestler
   */
  def makeWrestler(wrestlerName:String, wrestlerGender:String): Wrestler = {
    val rawText = urlToString("https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvprop=content&format=xml&titles=" + makeWikiLike(wrestlerName))
    val infobox = rawText.split("}}\\s*'''")(0)
    var myWrestler = new Wrestler(wrestlerName)
    myWrestler.gender = wrestlerGender
    myWrestler.birthdate = getBirthday(infobox)
    //    myWrestler.debut = getDebut(infobox)
    //      myWrestler.height = getSlice(infobox, "height = {{height|")
    //      myWrestler.weight = getSlice(infobox, "weight = {{convert|")
    return myWrestler
  }

  /*
  Retourne une String correspondant a ce qui vient entre une sous-chaine passee en parametre et le delimiteur passe en parametre dans une chaine passee en parametre
  Exemple: permettra par exemple de renvoyer 1975|07|17  a partir d'une chaine comme ".../>| weight = {{convert|130|lb|kg st|abbr=on}}<ref name="WEW"/>| birth_date  = {{birth date and age|1975|07|17}}<ref name=oww/>| birth_place =..."
   */
  def getSlice(originalText:String, sliceStart:String, sliceEnd:String): String ={
    if (!originalText.contains(sliceStart)) return ""
    var startOfKeyIndex = originalText.indexOf(sliceStart)
    if (!sliceStart.endsWith("|")) startOfKeyIndex += 1
    val startofSliceIndex = startOfKeyIndex + sliceStart.length
    val endOfSliceIndex = originalText.indexOf(sliceEnd, startofSliceIndex)
    var slice = originalText.slice(startofSliceIndex, endOfSliceIndex)
    return slice
  }

  def getBirthday(originalText:String):String ={
    var key = "irth date and age"
    var end = "}}"
    var birthday = getSlice(originalText, key, end)
    if (birthday != ""){
      birthday = birthday.replaceAll("\\|*[md]f=y(es)*\\|*", "")
    }
    else birthday = "UNKNOWN"
    return birthday
  }

  /*
  ne fonctionne pas encore
   */
  def getDebut(originalText:String):String ={
    var key = "debut"
    var end = "}}"
    var debut = getSlice(originalText, key, end)
    //    if (debut != ""){
    //      debut = debut.replaceAll("\\|*[md]f=y(es)*\\|*", "")
    //    }
    return debut
  }

  //TODO
  def getRetired(originalText:String):String ={
    return "UNKNOWN"
  }

  //TODO
  def getHeight(originalText:String):String ={
    return "UNKNOWN"
  }

  //TODO
  def getWeight(originalText:String):String ={
    return "UNKNOWN"
  }

  def populateDB(gender:String, wrestlerCategory:String): Map[String,Country] ={
    println("Populating wrestler database from wikipedia...")
    var countries:Map[String,Country] = Map()
    var femaleWrestlerNationalities = getPagesFromCategory(wrestlerCategory) //Recupere toutes les sous-categories d'une categorie wikipedia (ici Female professional wrestlers by nationality)
    for (i <-0 until femaleWrestlerNationalities.length) {
      val category = femaleWrestlerNationalities(i) //isole le nom de categorie (exemple "Category:American female professional wrestlers"
      val nationality = getNationalityFromCategory(category, gender) //recupere le nom de la nationalite (exemple "American" pour "Category:American female professional wrestlers"
      var tmpCountry = new Country(nationality)
      //      println(nationality+" wrestlers:") //Affichage pour debuggage, a retirer quand interface faite
      var wrestlerNames = getPagesFromCategory(category)
      for (j <-0 until wrestlerNames.length) {
        val wrestlerName = wrestlerNames(j)
        if (!wrestlerName.startsWith("Category:")){
          var tmpWrestler = makeWrestler(wrestlerName, gender)
          tmpWrestler.country = tmpCountry
          //          wrestlers = wrestlers :+ tmpWrestler
          tmpCountry.wrestlers = tmpCountry.wrestlers :+ tmpWrestler
          //          println("- "+tmpWrestler) //Affichage pour debuggage, a retirer quand interface faite
        }
      }
      countries = countries + (nationality.toLowerCase -> tmpCountry)
    }
    println("...done")
    return countries
  }

  /*
   - on cree un objet Country pour chaque nationalite presente dans la categorie wiki Category:Female professional wrestlers by nationality (https://en.wikipedia.org/wiki/Category:Female_professional_wrestlers_by_nationality)
   - on cree un objet Wrestler par catcheuse presente dans chacune des categories de pays, par exemple chaque catcheuse de la categorie Category:American female professional wrestlers (https://en.wikipedia.org/wiki/Category:American_female_professional_wrestlers)
   - chaque Country contient la liste des catcheuses de cette nationalite (liste d'objets Wrestler)
   - chaque Wrestler comporte une ref vers son Country
   - pour chaque Wrestler, actuellement, seules les infos de nom, de nationalite, et d'anniversaire sont recuperees
   - les Countries sont stockés dans une map, chaque objet Country a pour clé une String correspondant à sa nationalité en lowercase (et = au champ name de l'objet Country, majuscule mise à part)
   - interface utilisateur pas encore implementee
   => Actuellement, pour montrer le travail, on affiche simplement une liste du type:
           Swedish wrestlers:
        - Sarah Bäckman, female, Swedish, born 1991|12|8
        - LiLiCo, female, Swedish, born 1970|11|16
        - Jenny Sjödin, female, Swedish, born 1985|8|2
    */
  def main(args: Array[String]): Unit = {

    //    var wrestlers:List[Wrestler] = List()
    val gender = "female"
    val categoryName = "Category:Female professional wrestlers by nationality"
    var countryMap = populateDB(gender, categoryName)
    var input = ""
    breakable {
      while (input != "exit") {
        println("Type wrestler nationality (or \"list\" to get available nationalities, or \"exit\" to exit program):")
        input = scala.io.StdIn.readLine().toLowerCase
        if (input == "exit") break
        if (input == "list") {
          println(countryMap.keys.mkString("; "))
        }
        else if (countryMap.contains(input)) println(countryMap(input))
        else println("No wrestler from this country was found in the database")
      }
    }
  }
  //TEST
  //println(getNationalityFromCategory("Category:New Zealand female professional wrestlers", gender))
}

