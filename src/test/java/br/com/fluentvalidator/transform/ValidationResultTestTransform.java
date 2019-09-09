package br.com.fluentvalidator.transform;

import br.com.fluentvalidator.ValidationResult;
import br.com.fluentvalidator.exception.Error;
import br.com.fluentvalidator.transform.ValidationResultTransform;

public class ValidationResultTestTransform implements ValidationResultTransform<String> {

	@Override
	public String transform(final ValidationResult validationResult) {
		final StringBuilder sb = new StringBuilder();
		for (final Error error : validationResult.getErrors()) {
			sb.append(String.format("%s\n", error.getMessage()));
		}
		return sb.toString();
	}

}
