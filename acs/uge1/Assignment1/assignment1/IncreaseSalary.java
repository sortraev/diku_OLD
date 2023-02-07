package assignment1;

public class IncreaseSalary implements Mutator<Employee> {
  public void mutate(Employee emp) {
	int emp_age = emp.getAge();
	if (emp_age > 40) {
	  emp.setSalary(emp.getSalary() + emp_age/2);
	}
  }
}
