import tactic.ext
import data.prod

@[ext]
structure digraph(V: Type) :=
  (has_arc : V -> V -> Prop)

namespace digraph
  def from_rel{V: Type} (rel: V -> V -> Prop): digraph V := {
    has_arc := rel
  }

  def complete_graph(V: Type): digraph V := {
    has_arc := λ i j, true
  }

  def empty_graph(V: Type): digraph V := {
    has_arc := λ i j, false
  }

  def inv{V: Type}(g: digraph V): digraph V := {
    has_arc := λ i j, g.has_arc j i
  }

  def union{V: Type}(g1 g2: digraph V): digraph V := {
    has_arc := λ i j, (g1.has_arc i j) ∨ (g2.has_arc i j)
  }

  def mem{V: Type}(e: V × V)(g: digraph V): Prop := g.has_arc e.fst e.snd

  instance{V: Type}: has_union (digraph V) := ⟨ union ⟩
  instance{V: Type}: has_mem (V × V) (digraph V) := ⟨ mem ⟩
  instance{V: Type}: has_inv (digraph V) := ⟨ inv ⟩
end digraph

lemma inv_inv{V: Type}: ∀ (g: digraph V), g.inv.inv = g := begin
  intro g,
  simp [digraph.inv],
  apply digraph.ext,
  simp,
end

lemma inv_mem{V: Type}{g: digraph V}{e: V × V}: e ∈ g ↔ (prod.swap e) ∈ g⁻¹ := begin
  apply iff.intro,
  {
    intro h,
    from h,
  },
  {
    intro h,
    from h,
  }
end

lemma union_mem{V: Type}: ∀ (g1 g2: digraph V), ∀ (e: V × V), ((e ∈ g1) ∨ (e ∈ g2)) ↔ e ∈ (g1 ∪ g2) := begin
  intros g1 g2 e,
  apply iff.intro,
  repeat {
    intro h,
    cases h with h1 h2,
    apply or.intro_left,
    from h1,
    apply or.intro_right,
    from h2,
  },
end

lemma union_comm{V: Type}{g1 g2: digraph V}: g1 ∪ g2 = g2 ∪ g1 := begin
  ext a b,
  from or.comm,
end

lemma mem_eq{V: Type}{g1 g2: digraph V}: (∀(e: V × V), e ∈ g1 ↔ e ∈ g2) -> g1 = g2 := begin
  intro h,
  ext a b,
  apply iff.intro,
  {
    intro h2,
    have h3: (prod.mk a b) ∈ g1, from h2,
    from (h (prod.mk a b)).elim_left h3,
  },
  {
    intro h2,
    have h3: (prod.mk a b) ∈ g2, from h2,
    from (h (prod.mk a b)).elim_right h3,
  }
end

theorem inv_union{V: Type}: ∀ (g₁ g₂: digraph V), (g₁ ∪ g₂)⁻¹ = (g₁⁻¹ ∪ g₂⁻¹) := begin
  intros g1 g2,
  apply mem_eq,
  intro e,
  apply iff.intro,
  repeat {
    intro h,
    cases h with h1 h2,
    apply or.intro_left,
    from h1,
    apply or.intro_right,
    from h2
  },
end
