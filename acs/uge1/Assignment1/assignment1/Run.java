
package assignment1;

public class Run {
  public static void main(String[] args) {

	Employee anders = new Employee("AnDErS", 26, 223);
	Employee jan    = new Employee("jan", 63, 400);


	/* 
	IncreaseSalary inc = new IncreaseSalary();

	System.out.println(jan.toString());
	inc.mutate(jan);
	System.out.println(jan.toString());

	LowerCaseName lower = new LowerCaseName();

	System.out.println(anders.toString());
	lower.mutate(anders);
	System.out.println(anders.toString());

	*/

	LongestName longest = new LongestName();

	System.out.println("foo: " + longest.combine(longest.get(anders), longest.get(jan)));

  }
}
