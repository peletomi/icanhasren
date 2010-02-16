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

Some patterns support limiting the action to a particular part of the file
name. In this cases the selector character is located after the pattern character.
For example:

  [Xe] - limits the extraction to the extension

The pattern character always sets the casing, not the selector character.

Selector characters are:

  * f - whole file name

  * n - name without extension

  * e - extension

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

  * [X:start]   - string - returns the substring denoted by start. Start is a
                           one based index. If start is negative, then the
                           index is from the end of the string. If the start
                           is past or before the string, then no or all
                           characters will be returned.  The extractor pattern
                           supports selector characters.

                  Examples: - [X:3] return the substring of the file name from
                                    the third character

                            - [X:-2] return the substring from the second
                                     character from the end of the string

                            - [Xxe:2] return the from the second character of
                                      the extension, with the first letter
                                      upper cased the others lower cased

  * [X:start,length]    - string - returns the substring denoted by start with
                                   the given length. If the length is
                                   negative, then the substring before start
                                   with the given length will be returned.

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

