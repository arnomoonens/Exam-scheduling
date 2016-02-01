# Exam-scheduling
Exam scheduling in Prolog (Declarative programming project).

## Loading the code and problem instance
First, the *Swipl* program should be started. This can be done in multiple ways depending on your installation. On Linux or Mac, one can use Terminal to go to the directory of my project and execute `swipl` there.
Next, the `load_code.pl` file should be consulted. If you started *Swipl* using *Terminal* in the project directory, it is sufficient to execute `consult(load_code)`. The instance to be used also needs to be consulted (such files aren't included in this repository, please see [this](https://ai.vub.ac.be/sites/default/files/small_instance.txt), [this](https://ai.vub.ac.be/sites/default/files/large_short_instance.txt) and [this](https://ai.vub.ac.be/sites/default/files/large_long_instance.txt) file. This can be done before or after consulting the `load_code.pl` file.
When this is all done, it is possible to run the predicates listed.

## Usable predicates
- `is_valid(?S)`
- `cost(+S,?Cost)`
- `violates_sc(+S,-SC)`
- `find_optimal(-S)`
- `is_optimal(?S)`
- `find_heuristically(-S)`
- `find_heuristically(-S,+T)`
- `pretty_print(+S)`
- `pretty_print(+SID,+S)`
