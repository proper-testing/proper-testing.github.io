Summary: For the effective use of PropEr
kate: replace-tabs-save on; replace-tabs on; tab-width 8;

Tips on testing stateful code
------------------------------

* When writing properties for stateful systems, we should not forget to include
  some clean-up code that will be executed after a command sequence is run.
  This is important because side-effects from previous tests may cause
  subsequent tests to behave non-deterministically.

* When specifying a model for the system under test, we should not forget to
  validate it. That is, think about what is actually tested in the properties
  and, also, how often each operation is tested. One suggestion is to use the
  `proper_gen:pick/2` function so as to inspect the content of generated
  testcases for different values of the `size` parameter. When tests start
  to pass, it is important to collect statistics about testcase distribution
  using functions such as `proper:aggregate/2`.

* Parallel testing with PropEr is currently not fully automated. The abstract
  state machine that we use for sequential testing can also be used to
  generate command sequences that will be executed in parallel to test for
  race conditions. However, the actual command interleavings depend on the
  Erlang scheduler, which is too deterministic. For PropEr to be able to detect
  race conditions, the code of the system under test should be instrumented
  with `erlang:yield/0` calls to the scheduler.

* Abstract state machines that will be used for parallel testing should not
  contain very strict preconditions. When generating a parallel testcase, PropEr
  has to ensure that preconditions are satisfied in all possible interleavings
  of the concurrent command sequences. Otherwise, an (uncaught) exception might
  be raised during parallel execution and lead to test failure. It is usually
  possible to move constraints from preconditions to postconditions. When
  removing a constraint from a precondition, we need to catch the exceptions
  raised from invalid operations. Then, when adding the constraint to a
  postcondition, we should check that when executing an invalid operation, the
  expected kind of exception is indeed raised.
