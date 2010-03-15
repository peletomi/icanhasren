This tool renames files and directories based on patterns. 

Patterns
========

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

Full file name
--------------

  Pattern: [F], [FF], [ff], [Ff]

  Description: returns the whole file name

File name
---------

  Pattern: [N], [NN], [nn], [Nn]

  Description: returns the file name without the extension

Extension
---------

  Pattern: [E], [EE], [ee], [Ee]

  Description: returns the extension

Counters
--------

  There are two types of counters: string, integer. Multiple different counters
  (with different properties) can be specified in a pattern. It is possible to
  use one counter several times in a pattern, it will be incremented only
  once. The general format of the pattern is:

    [Cid:format,step]

  Pattern: [C]

  Description: returns a counter value, counting from 1, with the default
               format

  Pattern: [C:format]

  Description: the format influences multiple properties. First it chooses
               between numerical, and character counters. If the format is all
               digits it will be a numerical, otherwise it will be a character
               counter. In both cases it sets the start value of the counter
               as well.

               In the case of a numerical counter the format sets the padding
               length as well. For example: 0001, the counter will be padded
               with nulls to a length of 4.

               In the case of character counters it is possible to set the
               character case as usual, with varying the casing of the C.

  Examples: * [C:2]     start from 2 do not change formatting

            * [C:001]   start from one, pad the result with 0 to a length of 3

            * [C:a]     use a character counter

            * [CC:A]    use a character counter, with upper case characters

            * [Cc:bb]   use a character counter, start from bb, first character
                        is upper case others lower case

  Pattern: [C:format,steps]
  
  Description: format is the same as before. The steps describe the difference
               between two counts

  Pattern: [Cid]

  Description: id is a numerical value, which gives a name to the counter
               allowing multiple counters to co-exist. Only the first
               occurence of the counter has to be configured.

  Examples: * [C1:001][C2:a][C1]   the first counter is a numerical one,
                                   with a padded format. The second is a
                                   character counter. The first counter is
                                   used again, here only the id is given.

Extraction
----------

  Pattern: [X:start]
  
  Description: returns the substring denoted by start. Start is a one based
               index. If start is negative, then the index is from the end of
               the string. If the start is past or before the string, then no
               or all characters will be returned.  The extractor pattern
               supports selector characters.

  Examples: * [X:3]   return the substring of the file name from 
                      the third character

            * [X:-2]  return the substring from the second character
                      from the end of the string

            * [Xxe:2] return the from the second character of
                      the extension, with the first letter upper cased the
                      others lower cased

  Patter: [X:start,length]
  
  Description: returns the substring denoted by start with the given length.
               If the length is negative, then the substring before start with
               the given length will be returned.

Collisions
==========

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
=======

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

