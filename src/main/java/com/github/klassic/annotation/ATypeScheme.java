package com.github.klassic.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface ATypeScheme {
    String[] tvs();
    String type();
}
