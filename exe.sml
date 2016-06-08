use "funpp.sml";

fun execute(file)= 
    FunPP.print_prog(Parse.parse ("", TextIO.openIn (file)))
