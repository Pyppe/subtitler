package fi.pyppe.subtitler

// E.g. Language(eng, en, English)
case class Language(id: String, code: String, name: String)
object Languages {

  val Available: Set[Language] = {
    io.Source.fromInputStream(getClass.getResourceAsStream("/languages.tsv"), "utf-8").
      getLines.filterNot(_.startsWith("#")).map(_.split('\t')).collect {
        case Array(id, code, name, uploadEnabled, webEnabled) if uploadEnabled != "0" || webEnabled != "0" =>
          Language(id, code, name)
      }.toSet
  }

  def find(input: String): Option[Language] = {
    val q = input.toLowerCase
    Available.find(lang => lang.id == q ||
                           lang.code == q ||
                           lang.name.toLowerCase == q)
  }


}
