package assignment1;

public class LowerCaseName implements Mutator<Employee> {
  public void mutate(Employee emp) {
	emp.setName(emp.getName().toLowerCase());
  }
}
