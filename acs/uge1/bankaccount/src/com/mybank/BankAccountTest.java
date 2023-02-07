package com.mybank;

import static org.junit.Assert.*;

import org.junit.*;
import java.util.Random;

/**
 * {@link BankAccountTest} tests the {@link Account} interface.
 * 
 * @see Account
 */
public class BankAccountTest {

	/** The Constant TEST_CPR. */
	private static final int TEST_CPR = 1234567890;

	/** The local test. */
	private static boolean localTest = false;

	/** The client. */
	private static Account client;

	/**
	 * Sets up before class.
	 *
	 * @throws Exception
	 *             the exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		if (localTest) {
			client = new BankAccount(TEST_CPR);
		} else {
//      TODO: simply run a server here.
//      BankAccountHTTPServer.init_server(BankAccountHTTPServer.DEFAULT_PORT,
//                                         BankAccountHTTPServer.PROPERTY_KEY_SERVER_PORT);
			client = new BankAccountHTTPProxy("http://localhost:8081");
		}
	}

	@After
  @Before
	public void cleanUp() throws AccountException {
	  // reset client account balance before and after each test.
		client.withdraw(client.getBalance());
	}


	/**
	 * Test balance zero.
	 * 
	 * @throws AccountException
	 *             if the balance cannot be obtained
	 */
	@Test
	public void testBalanceZero() throws AccountException {
		assertEquals(0, client.getBalance());
	}

	/**
	 * Test correct balance.
	 */
	@Test
	public void testCorrectBalance() throws AccountException {

		// Tests balance after series of deposit-withdraw invocations
    int expected = 0;
    assertEquals(expected, client.getBalance());

    client.deposit(42);
    expected += 42;
		assertEquals(expected, client.getBalance());

		client.deposit(1337);
		expected += 1337;
		assertEquals(expected, client.getBalance());

		client.withdraw(1000);
		expected -= 1000;
		assertEquals(expected, client.getBalance());


	}

	@Test
	public void testManyTransactions() throws AccountException {

		int num_transactions = localTest ? 1000000 : 1000;

		Random rand = new Random();
		int expected = 0;

		// execute num_transactions number of transactionsh
		// with an expected 1/5 of withdrawals.
		for (int i = 0; i < num_transactions; i++) {
			int tmp = rand.nextInt(4732);

			if (rand.nextInt(5) == 0) {
				tmp = Math.min(client.getBalance(), tmp);
				client.withdraw(tmp);
				expected -= tmp;
			} else {
				client.deposit(tmp);
				expected += tmp;
			}
		}

		assertEquals(expected, client.getBalance());
	}

	@Test(expected = AccountException.class)
	public void testBalanceOverflow() throws AccountException {
	  client.deposit(2_000_000_000);
		client.deposit(1_000_000_000); // this deposit should produce a 32 bit signed int overflow.
	}

	/**
	 * Test overdraft.
	 */
	@Test(expected = AccountException.class)
	public void testOverDraft() throws AccountException {

		// BankAccountException should be thrown if we try to withdraw amount
		// grater than balance
		client.withdraw(client.getBalance() + 42);
	}

	/**
	 * Test negative withdraw.
	 */
	@Test(expected = AccountException.class)
	public void testNegativeWithdraw() throws AccountException {
		client.withdraw(-42);
	}

	/**
	 * Test negative deposit.
	 */
	@Test(expected = AccountException.class)
	public void testNegativeDeposit() throws AccountException {
		// an AccountException is thrown when a negative deposit is attempted.
		client.deposit(-42);
	}

	/**
	 * Tear down after class.
	 *
	 * @throws Exception
	 *             the exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		if (!localTest) {
			((BankAccountHTTPProxy) client).stop();
		}
		else {
		}
	}
}