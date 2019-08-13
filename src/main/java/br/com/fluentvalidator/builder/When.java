package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface When<T, P> {

	Must<T, P> must(final Predicate<P> predicate);
	
}
