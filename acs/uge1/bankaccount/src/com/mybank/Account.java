package com.mybank;

/**
 * {@link Account} specifies the account contract.
 */
interface Account {

	/**
	 * Returns the balance of the account.
	 *
	 * @return the balance
	 * @throws AccountException
	 *             the bank account exception
	 */
	int getBalance() throws AccountException;

	/**
	 * Adds n units of currency to the balance.
	 *
	 * @param n
	 *            the number of units of currency
	 * @throws AccountException
	 *             the bank account exception
	 * @throws IllegalArgumentException
	 *             (if n < 0)
	 */
	void deposit(int n) throws AccountException;

	/**
	 * Subtracts n units of currency from the balance.
	 *
	 * @param n
	 *            the number of units of currency
	 * @throws AccountException
	 *             the bank account exception
	 * @throws IllegalArgumentException
	 *             (if n < 0)
	 */
	void withdraw(int n) throws AccountException;
}
