#### Definindo a função garch_sim para simular dados de um modelo garch(p,q) ####

function garch_sim(n, α, β)
    order_max = max(length(α), length(β))
    ϵ = randn(n + 1)
    ret = repeat([0], outer = n+1)
    σ² = repeat([0], outer = n+1)

    for i in (order_max + 1):(n + order_max)
        sum_alpha = 0
        sum_beta = 0
        for p in 2:(length(α)) 
            sum_alpha = sum_alpha + α[p]*(ret[i-p+1])^2
        end
        for q in 1:(length(β))
            sum_beta = sum_beta + β[q]*σ²[i-q] 
        end
        println(typeof(sum_beta))
        println(typeof(sum_alpha))
        println(typeof(α[1]))

        σ²[i] = α[1] + sum_alpha + sum_beta
        ret[i] = sqrt(σ²[i])*ϵ[i]
    end
    return(ret[(order_max + 1):(n + order_max)])
end

garch_sim(10, [.2, .3], [.1])
