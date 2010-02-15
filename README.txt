This tool renames files and directories based on patterns. 

Patterns
--------

There are two kinds of patterns: literal, variable. The latter ones are
surrounded by a pair of brackets i.e.: []. The patterns which handle string
values support the change of case:

  * x  - unchanged
  * XX - upper case
  * Xx - first character upper case, others lower case
  * xx - lower case

The following patterns are available:

  * [F] - string - returns the whole file name

  * [N] - string - returns the file name without the extension

  * [E] - string - returns the extension

  * [C] - int / string - returns a counter value. By default counting from 1
          format is not changed

  * [C:format] - int / string - the format defines multiple things: counter
                 type (integer, string), start value, character case, padding.

                 Examples: - [C:2] start from 2 do not change formatting
                    
                           - [C:001] start from one, pad the result with 0 to
                                     a length of 3

                           - [C:a] use a character counter

                           - [C:AA] use a character counter, with upper case
                                    characters

  * [C:format,steps] - int / string - format is the same as before. The steps
                                      describe the difference between two
                                      counts

  * [X]

  * [X:1]

  * [X:1,12]

  * [X:1,-1,e] e f n

Collisions
----------

There are three types of collisions: existing files, new names colliding, and
new names colliding with old names. In show mode these three types are
displayed along with the old and new names. The following symbols are used:

  * - existing files would be overwritten
  ! - the new name of the file is the same as the new name of some other file(s)
  = - the new name of the file is the same as the old name of some other file

Existing files will not be overwritten, unless the force command line option
is given.

If a new name collision is detected the program exits, as this is certainly
not the desired outcome.

If an old name collision is detected, the application tries to solve this by:
re-ordering the file renames, inserting temporary renames given there is a
circular dependency between the renames.

Options
-------

  -h       --help        
               Usage information.

  -s       --show-only
               Only show renames.

  -l FILE  --log=FILE
               Log rename actions to file. The format of the file is:
               [old name]:[new name]

  -i FILE  --input=FILE  
               Load renames from file. The file format should be the same as
               the log is writing.

  -u FILE  --undo=FILE
               Undo renames from a log file. The old names in the files will
               be used as new, and the new as old names.

  -f       --force
               Force renaming. Existing files will be overwritten.

