# Changes to consider

## Kinds

The representation of kinds as sets of sorts is problematic when sort graphs are merged. The function extended-kind returns the correct new representation for a kind that was created for one of the input sort graphs, but it must be called explicitly, which is error-prone.

An alternative would be to use a struct (kind-of sort) and expand this into the full set when tests or comparisons need to be done.

## Term creation

The API for term creation is far from definitive. Error handling in particular needs to be defined better.

## Merging

Merging signatures or varsets implies merging their sort graphs, which ends up getting done twice when contexts are merged.
