object FileMatcher {
    val filesHere = (new java.io.File("/home/pk/GIT/Scala-Fun/hello-world/src/main/scala")).listFiles


    private def filesMatching (matcher: String => Boolean) = {
    for (file <- filesHere; if matcher(file.getName))
        yield file
    }
    
    def filesEnding(query: String) =
        filesMatching(x => x.endsWith(query))
    
    def filesContaining(query: String) =
        filesMatching(_.contains(query))
    
    def filesRegex(query: String) =
        filesMatching(_.matches(query))
}