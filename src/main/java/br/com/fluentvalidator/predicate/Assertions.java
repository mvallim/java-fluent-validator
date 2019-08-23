package br.com.fluentvalidator.predicate;

import java.util.Objects;

import br.com.fluentvalidator.ValidationContext;

final class Assertions {

	private Assertions() {
		super();
	}

	static <T> void checkNotNull(final T obj, final String predicateName) {
		if (Objects.isNull(obj)) {
			ValidationContext.remove();
			throw new NullPointerException(String.format("predicate '%s' could not evaluate null value", predicateName));
		}
	}

}
