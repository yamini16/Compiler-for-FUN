structure Run = 
struct
   fun run filename = Parser.parse ("", TextIO.openIn (filename))
end
