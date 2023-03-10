package com.acertainbookstore.business;

/**
 * {@link Book} implements the book data-structure that a bookstore client works
 * with.
 */
public interface Book {

	/**
	 * Gets the ISBN of the {@link Book}.
	 *
	 * @return the ISBN
	 */
	int getISBN();

	/**
	 * Gets the title of the {@link Book}.
	 *
	 * @return the title
	 */
	String getTitle();

	/**
	 * Gets the author of the {@link Book}.
	 *
	 * @return the author
	 */
	String getAuthor();

	/**
	 * Gets the price of the {@link Book}.
	 *
	 * @return the price
	 */
	float getPrice();
}
