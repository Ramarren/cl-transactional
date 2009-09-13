# cl-transactional

## Initial notes

This is a proof of concept STM system implemented in a quite naive and likely buggy way. I wrote this mostly to get an idea how such things can be implemented, and have not used it since.

For any serious Common Lisp STM needs the [cl-stm](http://common-lisp.net/project/cl-stm/) system is probably much better. That said, since I dropped it on GitHub I might just as well document it. I suppose there is a possibility that it might be informational to someone.

## Basic ideas

This system is much simpler than aforementioned cl-stm because it puts more responsibility on the user. While cl-stm uses the code walker to capture side effects into transactions, this system assumes all information is shared through special references to immutable data, called tvars following Haskell example. Of course, unlike in Haskell it is not possible to enforce neither the immutability of referenced data or that tvars will be accessed though transaction logs.

Immutable datastructures are available though, for example, [FSet](http://common-lisp.net/project/fset/) library.

Transactions as implemented here are in principle composable.

## Dependencies

- SBCL

`sb-thread` is used directly. `bordeaux-threads` does not have non-waiting `with-mutex`.

- Iterate
- Alexandria

Tests (what few there are) require also:

- stefil
- fset

## Reference

Note that transaction failure is communicated using signals. Creating a `t` condition handler will break them.

Function `make-tvar &optional value`

Create a transactional variable with an optional initial value.

Function `get-tvar tvar &optional transaction-log wrapping-logs`

Function `tvar-ref tvar`

Mostly synonymous. Optional arguments of the first form are used internally and default to thread-local special bindings. Access contents of a transactional variable. Can be used outside of transaction.

Function `put-tvar tvar new-value &optional transaction-log wrapping-logs`

Function `(setf tvar-ref) new-value tvar`

Mostly synonymous. Optional arguments of the first form are used internally and default to thread-local special bindings. Sets contents of a transactional variable. Can be used outside of transaction.

Function `retry-transaction`

Restart transaction. Will suspend the thread until one of the tvars which were accessed so far in the transaction is changed.

Function `with-retry-transaction &body body`

Execute body within transaction. It will be retried if any tvars which were read during it have been changed.

Function `with-orelse-transaction &body (alternative-body)*`

Execute bodies withing orelse transaction, ie. if the first fails, the second is immediately attempted and so on. If all fail and there was at least one explicit (retry-transaction) the thread will be suspended until some referenced tvar changes.
