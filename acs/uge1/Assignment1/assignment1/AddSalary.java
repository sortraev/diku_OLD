package assignment1;

public class AddSalary implements Combinator<Employee, Integer> {
  public void mutate(Employee emp) {
	emp.setName(emp.getName().toLowerCase());
  }

  public Integer neutral() {
    return 0;
  }

  public Integer combine(Integer x, Integer y) { 
    return x + y;
  }


  public Integer get(Employee emp) { 
    emp.getSalary();
  }

}
