class: middle, center

# Chapter 2
## Mathematical Preliminaries

---
layout: true 

## Sets

---

$$ \left\lbrace 37, 51\right\rbrace $$

--

$$ \varnothing $$

--

$$ \mathbb{N} = \left\lbrace 1, 2, 3, \dots \right\rbrace $$

---

$$ \left\lbrace 2 \cdot s | s \in S; s = 0 \mod 2  \right\rbrace $$

--

```python
[2*s for s in S if val % 2 == 0]
```

---

### Cartesian Product

The *cartesian product* of $X$ and $Y$

$$X \times Y$$

```python
[(a, b) for a in X for b in Y]
```

---

### Subset

$S$ is *a subset* of $T$

$$ S \subset T $$

if all elements $s\in S$ are also element of $s\in T$ 

---
layout: true

## Relation

---

A  _n-place_ *relation* on collection $S_{1}, S_{2}, \ldots, S_{n}$
is a subset $R$ of the cartesian product

$$ R \subset S_{1} \times S_{2} \times \ldots \times S_{n} $$

---

### Predicate

A *predicate* is a 1-place relation on a set $S$.

---

### Binary Relation

A *binary relation on U* is a 2-place relation on $U \times $U$.
