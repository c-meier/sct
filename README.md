# Student Correction Transformer (SCT)

SCT is a simple tool to extract code destined to students or for correction from a master file.

For the moment only language which use `//` to comment code are supported.

## Example

Given the following master file (`demo.scala`):

```scala
import spark

object MainApp {
    //![
    def methodToImplement() = someImplementation
    //!-
//    def methodToImplement() = ???
    //!]

    //![-
//    // TODO: Some instruction given to the students, but missing from the correction.    
    //!]
    def main(args: Array[String]) {
        //![
        Code only available in the correction, with no alternative for student.
        //!-]  
    }
}
```

Running `sct -s demo.scala` outputs the following code (comment the part for the correction and uncomment the part for the student):

```scala
import spark

object MainApp {
    //![
//    def methodToImplement() = someImplementation
    //!-
    def methodToImplement() = ???
    //!]

    //![-
    // TODO: Some instruction given to the students, but missing from the correction.    
    //!]
    def main(args: Array[String]) {
        //![
//        Code only available in the correction, with no alternative for student.
        //!-]  
    }
}
```

Running `sct -os demo.scala` outputs the following code (only the part for the student):

```scala
import spark

object MainApp {
    def methodToImplement() = ???

    // TODO: Some instruction given to the students, but missing from the correction.    
    def main(args: Array[String]) {
    }
}
```

Running `sct -co demo.scala` outputs the following code (only the part for the correction):

```scala
import spark

object MainApp {
    def methodToImplement() = someImplementation

    def main(args: Array[String]) {
        Code only available in the correction, with no alternative for student.
    }
}
```

## Tip

If you want to transform every scala file in a directory into its student version, you can use the following command:

```sh
fd -e scala --ignore-file=<(sct -so .sctignore) -x sh -c "sct -so {} | sponge {}"
```

If you want to copy a transformed hierarchy of files into a new directory, you can use the following command:

```sh
DEST_DIR="../student2021"; fd -t f --ignore-file=<(sct -so .sctignore) -x sh -c "mkdir -p $DEST_DIR/{//}; sct -so {} > $DEST_DIR/{}"
```

## Build haskell

```sh
cabal install exe:sct --install-method=copy --overwrite-policy=always --installdir=dist
stack install --local-bin-path=dist
```
