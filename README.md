# Trump Tweet Generator

## Authors
Andy Monroe and Gage Benne

## Dataset
We pulled our dataset from the trump_tweet_data_archive hosted on GitHub right [here](https://github.com/bpb27/trump_tweet_data_archive). Once collected in .json format, we convert it into a simplified format suitable for our purposes using a simple python script.

If we decide to extend this project in the future, we will likely update it to pull directly from Twitter's API.

## Language
We chose the Haskell programming language for our project, due to our shared experience with mathmatics, and our general affinity for the functional style offered by Haskell. Additionally, we thought it would make for an interesting challenge that would set our solution apart from those of our peers.

## Process
At a high level, our Hidden Markov Model uses a [Data.Map](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map.html) data structure to pair tuples of words or punctuation with a list of possible succeeding words or punctuation.

### Learning
Our Hidden Markov Model learns by taking in the next three tokenized words in a stream, putting the first two into a tuple, and finding or creating the matching entry in the Map. The third token is then appended to the list associated with the key.

### Generating
Two generate text, we build a tuple of tokens (which can each be a Nothing, if appropriate, like for a first word), and look up that key in our Map. We then select a randomized element from the list associated with that key as our succeeding token.

If a key is not found, we try a series of alternate keys based on the original key, until we reach the default (Nothing, Nothing) case. Our process guarentees that a (Nothing, Nothing) key always has a non-empty list, so this can be defaulted to when a key is not found.

## Results
We found our results to be HUGELY successful.

### Un-Seeded Tweet Examples
Tweets generated without any user-defined starter text. Disclaimer: These tweets have no character limit!
```
tweet 1
```
```
tweet 2
```
```
tweet 3
```

### I AM ...
Generated tweets with starting words "I AM"
```
tweet 1
```
```
tweet 2
```
```
tweet 3
```

### AMERICA IS ...
Generated tweets with starting words "AMERICA IS"
```
tweet 1
```
```
tweet 2
```
```
tweet 3
```

### BOB MUELLER ...
Generated tweets with starting words "BOB MUELLER"
```
tweet 1
```
```
tweet 2
```
```
tweet 3
```

### MAKE AMERICA ...
It has to get this one right...
```
tweet 1
```
```
tweet 2
```
```
tweet 3
```

