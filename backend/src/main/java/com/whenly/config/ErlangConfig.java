package com.whenly.config;

import com.whenly.service.ErlangBackendAPI;
import com.whenly.service.SharedStringList;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ErlangConfig {

    @Bean
    public SharedStringList sharedStringList() {
        return new SharedStringList();
    }

    @Bean
    public ErlangBackendAPI erlangBackendAPI() throws Exception {
        String javaNodeName = "java_backend@10.2.1.11"; // Configurabile da application.properties
        String cookie = "whenly";
        return new ErlangBackendAPI(javaNodeName, cookie);
    }
}
