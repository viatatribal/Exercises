local
  (* Get all the contents from the txt file *)
  fun getContent file =
      let
        val f = TextIO.openIn file
        val s = TextIO.inputAll f
        val _ = TextIO.closeIn f
      in s end;

  (* Get a random number from 0 to size of list *)
  fun getNth fs =
      let
        val len = length fs
        val rn  = Random.rand
                      (0, Date.second(Date.fromTimeLocal(Time.now())))
        val pos = Random.randRange (0, len) rn
      in
        pos
      end;

  (* write from string list to txt file *)
  fun writeContent (file,    []) = TextIO.closeOut file
    | writeContent (file, f::fs) =
      (  TextIO.output(file, (f ^ "\n"))
       ; writeContent (file, fs));

in
(* The main function *)
(* It reads the whole txt file and stores it in a vector *)
(* Then converts it to a lists of strings, each position in the *)
(* list is a line from the txt file *)
(* After that, we take a line ramdomly, print it and then *)
(* remove it from the list, finally saving it all in a txt file *)
fun getRandomLine file =
    let
      val fs = getContent file
      val words = String.tokens Char.isCntrl fs
      val pos = getNth words
      val project = List.nth(words, pos)
      val newWords = if pos = 0 then List.drop (words, pos+1)
                     else List.take (words, pos)
                          @ List.drop (words, pos+1)
      val out = TextIO.openOut file
    in
      ( TextIO.output(TextIO.stdOut, project ^ "\n")
      ; writeContent (out, newWords))
    end
end;
