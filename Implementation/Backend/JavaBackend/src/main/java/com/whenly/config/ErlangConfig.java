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


}
