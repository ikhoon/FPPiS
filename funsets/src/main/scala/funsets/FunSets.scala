package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * singletonSet(4) = x => x == 4
   * def isEqualTo4()
   *
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = _ == elem

  /**
   * x => x == 2 , x => x == 4, ...
   * x => x == 2 || x => x == 4
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = x => s(x) || t(x)

  /**
   * (x => x == 2 || x => x == 4) && (x => x == 5 || x => x == 6)
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = x => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = x => s(x) && !t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = x => s(x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   *
   * if ??? ??? // lower bound? return filter value
   * else if ??? ??? // filter value not contains return false
   * else iter(a - 1) // 만족했다
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) true
      else if (contains(s, a) && !contains(filter(s, p), a)) false
      else iter(a - 1)
    }
    iter(bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   * using iterate

  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) false
      else if (contains(s, a) && contains(filter(s, p), a)) true
      else iter(a - 1)
    }
    iter(bound)
  }
  */

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    * set = (1, 2, 3)
    * exists(set, _ > 2) == true [3 is bigger than 2] `equals` forall(set, !(_ > 2)) == false [3 is not less or equal than 2]
    * using forall, exists(s, p)
    * - some values in set satisfy the predicate(p) `equals` all values in set does not satisfy negative predicate(!p),
    * - exists(s, p) == true `equals` forall(s, !p) == false
    * - exists(s, p) == !forall(s, !p)
    * 어떤 조건을 만족하는 일부의 값이 있으면 모든 값이 그 조건을 만족하지 않는것이 아니다.
    * 어떤 조건을 만족하는 값이 없으면 모든값이 그 조건을 만족하지 않는다.
    * p -> q 참이면 대우 ~q -> ~p 참이
    */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, !p(_))

  /**
   * s1 = singletonSet(1)
   * f = x => x + 1
   * contains(map(s, f), 2)
   *
   * map(s1, f) => map(x => x == 1, y => y + 1) => ??? => x => x == 2
   * iterate all inner bounded values
   *
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    def iter(a : Int): Set = {
      if(a < -bound) x => false
      else if(contains(s, a)) union(iter(a - 1), singletonSet(f(a)))
      else iter(a - 1)
    }
    iter(bound)
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
