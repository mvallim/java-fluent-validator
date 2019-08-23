package br.com.fluentvalidator.builder;

import java.util.function.Predicate;

public interface When<T, P, W extends When<T, P, W>> {

	Must<T, P, W> must(final Predicate<? super P> predicate);
	
}
